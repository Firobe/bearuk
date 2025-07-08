module U = Yojson.Safe.Util
module FlagS = Set.Make (String)
module PathS = Set.Make (Fpath)

let p = Fmt.pr
let fpath x = Fpath.of_string x |> Result.get_ok
let root = Sys.getcwd () |> fpath
let relativize_path abs = Fpath.relativize ~root abs |> Option.get

let filter_flags args =
  let args = List.tl args in
  let to_exclude = [ "-c" ] in
  (* remove '-o x', excluded and last parameter *)
  let rec aux inc = function
    | [] -> ([], inc)
    | [ _ ] -> ([], inc)
    | "-o" :: _ :: tl -> aux inc tl
    | "-I" :: i :: tl -> aux (i :: inc) tl
    | i :: tl when String.starts_with ~prefix:"-I" i ->
        let i = String.sub i 2 (String.length i - 2) in
        aux (i :: inc) tl
    | o :: tl when String.ends_with ~suffix:".o" o -> aux inc tl
    | h :: tl when List.mem h to_exclude -> aux inc tl
    | h :: tl ->
        let l, inc = aux inc tl in
        (h :: l, inc)
  in
  let leftover_flags, includes = aux [] args in
  (includes, FlagS.of_list leftover_flags)

type tunit = {
  base_name : string;
  flags : FlagS.t;
  relpath : Fpath.t;
  includes : PathS.t; (* include paths are relative to root directory *)
  lang : [ `C | `CXX ];
}

let parse unit =
  let input = U.(member "file" unit |> to_string) |> fpath |> relativize_path in
  let filename_up =
    Fpath.rem_ext input |> Fpath.filename |> String.uppercase_ascii
  in
  let lang =
    match Fpath.get_ext input with
    | ".cpp" | ".cxx" | ".cc" -> `CXX
    | ".c" -> `C
    | s -> Fmt.invalid_arg "unknown ext %s" s
  in
  let includes, flags =
    U.(member "arguments" unit |> convert_each to_string) |> filter_flags
  in
  let cwd = U.(member "directory" unit |> to_string) |> fpath in
  let includes =
    List.map
      (fun i ->
        let fragment = fpath i in
        Fpath.(cwd // fragment) |> relativize_path |> Fpath.to_dir_path)
      includes
    |> PathS.of_list
  in
  { base_name = filename_up; flags; relpath = input; lang; includes }

let common_flags units =
  let cflags, cxxflags =
    List.partition_map
      (function
        | { lang = `C; flags; _ } -> Either.left flags
        | { lang = `CXX; flags; _ } -> Either.right flags)
      units
  in
  let rec reduce = function
    | [] -> FlagS.empty
    | [ x ] -> x
    | a :: b :: tl -> reduce (FlagS.inter a b :: tl)
  in
  let common_c = reduce cflags in
  let common_cxx = reduce cxxflags in
  let unique_only =
    List.map
      (fun u ->
        match u.lang with
        | `C -> { u with flags = FlagS.diff u.flags common_c }
        | `CXX -> { u with flags = FlagS.diff u.flags common_cxx })
      units
  in
  (unique_only, common_c, common_cxx)

let all_includes units =
  let cincludes =
    List.filter (function { lang = `C; _ } -> true | _ -> false) units
  in
  let cxxincludes =
    List.filter (function { lang = `CXX; _ } -> true | _ -> false) units
  in
  let reduce =
    List.fold_left
      (fun acc { includes; _ } -> PathS.union acc includes)
      PathS.empty
  in
  (reduce cincludes, reduce cxxincludes)

let write_section comment =
  p
    "\n\
     ################################################################################\n";
  p "# %s\n" comment;
  p
    "################################################################################\n"

let pp_flags fmt s = FlagS.iter (Fmt.pf fmt " %s") s

let write_unit name { base_name; relpath; flags; _ } =
  p "%s_SRCS-y += $(%s_BASE)/%a\n" name name Fpath.pp relpath;
  if not @@ FlagS.is_empty flags then
    p "%s_%s_FLAGS-y += %a\n" name base_name pp_flags flags

let write_makefile name units common_c common_cxx cincludes cxxincludes =
  let name_up = String.uppercase_ascii name in
  let pp_paths fmt s =
    PathS.iter (Fmt.pf fmt " -I$(%s_BASE)/%a" name_up Fpath.pp) s
  in
  write_section "Registration";
  p "$(eval $(call addlib,%s))\n" name;
  write_section "Flags";
  p "%s_CFLAGS-y += %a\n" name_up pp_flags common_c;
  p "%s_CXXFLAGS-y += %a\n" name_up pp_flags common_cxx;
  write_section "Includes";
  p "%s_CINCLUDES-y += %a\n" name_up pp_paths cincludes;
  p "%s_CXXINCLUDES-y += %a\n" name_up pp_paths cxxincludes;
  write_section "Sources";
  List.iter (write_unit name_up) units

let dedup_units units =
  let sorted =
    List.sort_uniq (fun a b -> Fpath.compare a.relpath b.relpath) units
  in
  let merge a b =
    {
      a with
      flags = FlagS.union a.flags b.flags;
      includes = PathS.union a.includes b.includes;
    }
  in
  let rec aux = function
    | a :: b :: tl when Fpath.equal a.relpath b.relpath -> merge a b :: aux tl
    | h :: tl -> h :: aux tl
    | [] -> []
  in
  aux sorted

type mode = Library | Application

let go name database mode =
  let name =
    match mode with Application -> "app" ^ name | Library -> "lib" ^ name
  in
  let units = Yojson.Safe.from_file database |> U.to_list in
  let units = List.map parse units |> dedup_units in
  let units, common_c, common_cxx = common_flags units in
  let cincludes, cxxincludes = all_includes units in
  write_makefile name units common_c common_cxx cincludes cxxincludes

open Cmdliner

let coco =
  let doc =
    "Path to the `compile_commands.json` file to use. By default, look in the \
     current directory"
  in
  Arg.(
    value
    & opt non_dir_file "./compile_commands.json"
    & info [ "f"; "file" ] ~docv:"PATH" ~doc)

let mode =
  let flags =
    [
      ( Library,
        Arg.info [ "lib" ]
          ~doc:"Generate build files for a library (the default)." );
      ( Application,
        Arg.info [ "app" ] ~doc:"Generate build files for a library." );
    ]
  in
  Arg.(value & vflag Library flags)

let name' =
  let doc =
    "Name of the application or library. Do not prefix with 'app' or 'lib'"
  in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)

let cmd =
  let term = Term.(const go $ name' $ coco $ mode) in
  let doc = "Generate Unikraft build files from a compile_commands.json file" in
  let info = Cmd.info ~doc "bearuk" in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
