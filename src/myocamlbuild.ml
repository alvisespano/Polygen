open Ocamlbuild_plugin
open Unix

let version = "1.1.0+dev"

let time =
  let tm = Unix.gmtime (Unix.time ()) in
  (* Printf.sprintf "%02d/%02d/%04d %02d:%02d:%02d UTC" *)
  Printf.sprintf "%04d%02d%02d"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday 
    (* tm.tm_hour tm.tm_min tm.tm_sec *)

let make_version _ _ =
  let cmd =
    Printf.sprintf "let version = %S\n\
                    let compile_time = %S\n"
      version time
  in
  Cmd (S [ A "echo"; Quote (Sh cmd); Sh ">"; P "version.ml" ])

let () =
  dispatch (function After_rules -> rule "version.ml" ~prod: "version.ml" make_version
                   | _ -> ())
