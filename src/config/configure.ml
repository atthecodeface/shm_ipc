(*
 * OWL - an OCaml numerical library for scientific computing
 * Copyright (c) 2016-2018 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

module C = Configurator

let sys_types = [
 ("linux", "linux");
 ("linux_elf", "linux");
 ("macosx", "osx");
 ("mingw64", "windows");
  ]

let disable_hugetlbfs = (Sys.getenv "DISABLE_HUGETLBFS" |> int_of_string)=1
let hugetlbfs_flag = if disable_hugetlbfs then "" else "-DHUGETLBFS"

let os_specifics = [
    ("linux",   ([hugetlbfs_flag;], ["-lhugetlbfs"]));
    ("osx",     ([], []));
    ("windows", ([], []));
  ]

let get_os c = 
  let sys = match (C.ocaml_config_var c "system") with
    | Some s -> s
    | None -> failwith ("system not defined in ocaml")
  in
  let os = match (List.assoc_opt sys sys_types) with
    | Some s -> s
    | None -> failwith ("system not known, so flags/libs cannot be determined "^sys)
  in
  match (List.assoc_opt os os_specifics) with
  | Some os_data -> os_data
  | None -> failwith ("unknown OS type - internal configure data structures are incorrect, bug... "^os)

let os_cflags os =
  let (cflags, _) = get_os os in
  cflags

let os_clibs os =
  let (_, clibs) = get_os os in
  clibs

let write_sexp filename sexp =
  Stdio.Out_channel.write_all filename ~data:(Base.Sexp.to_string sexp)

let get_default_libs c =
  let libs = [] in
  (os_clibs c) @ libs

let get_default_cflags c = 
  let flags = [] in
  (os_cflags c) @ flags

let () =
  C.main ~name:"shm_ipc" (fun c ->

    let libs = []
      @ get_default_libs c
    in

    let cflags = []
      @ get_default_cflags c
    in

    let conf : C.Pkg_config.package_conf = { cflags; libs } in

    write_sexp "c_flags.sexp"         Base.(sexp_of_list sexp_of_string conf.cflags);
    write_sexp "c_library_flags.sexp" Base.(sexp_of_list sexp_of_string conf.libs);
  )
