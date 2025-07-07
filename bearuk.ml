module U = Yojson.Safe.Util
module FlagS = Set.Make (String)

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
    | h :: tl when List.mem h to_exclude -> aux inc tl
    | h :: tl ->
        let l, inc = aux inc tl in
        (h :: l, inc)
  in
  let leftover_flags, includes = aux [] args in
  (FlagS.of_list includes, FlagS.of_list leftover_flags)

type tunit = {
  base_name : string;
  flags : FlagS.t;
  relpath : Fpath.t;
  includes : FlagS.t;
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
      (fun acc { includes; _ } -> FlagS.union acc includes)
      FlagS.empty
  in
  (reduce cincludes, reduce cxxincludes)

let write_unit name { base_name; relpath; flags; _ } =
  p "%s_SRCS-y += $(%s_BASE)/%a\n" name name Fpath.pp relpath;
  if not @@ FlagS.is_empty flags then (
    p "%s_%s_FLAGS-y +=" name base_name;
    FlagS.iter (p " %s") flags;
    p "\n")

let write_makefile name units common_c common_cxx cincludes cxxincludes =
  let name_up = String.uppercase_ascii name in
  p "# Registration\n";
  p "$(eval $(call addlib,%s))\n\n" name;
  if not @@ FlagS.is_empty common_c then (
    p "# Common C flags\n";
    p "%s_CFLAGS-y += " name_up;
    FlagS.iter (p " %s") common_c;
    p "\n");
  if not @@ FlagS.is_empty common_cxx then (
    p "# Common C++ flags\n";
    p "%s_CXXFLAGS-y += " name_up;
    FlagS.iter (p " %s") common_cxx;
    p "\n");
  if not @@ FlagS.is_empty cincludes then (
    p "# All C include paths\n";
    p "%s_CINCLUDES-y += " name_up;
    FlagS.iter (p " %s") cincludes;
    p "\n");
  if not @@ FlagS.is_empty cxxincludes then (
    p "# All C++ include paths\n";
    p "%s_CXXINCLUDES-y += " name_up;
    FlagS.iter (p " %s") cxxincludes;
    p "\n");
  p "\n";
  p "# Source files and their specific flags\n";
  List.iter (write_unit name_up) units

let go name database =
  let units = Yojson.Safe.from_file database |> U.to_list in
  let units = List.map parse units in
  let units, common_c, common_cxx = common_flags units in
  let cincludes, cxxincludes = all_includes units in
  write_makefile name units common_c common_cxx cincludes cxxincludes

let () =
  try
    let name = Sys.argv.(1) in
    let database = Sys.argv.(2) in
    go name database
  with _ -> p "Usage: bearuk NAME PATH_TO_COMPILE_COMMANDS\n"
