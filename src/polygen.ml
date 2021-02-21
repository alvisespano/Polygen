(*
 * Polygen
 * main.ml: main code
 *
 * Alvise Spano' (2002-03)
 *)

open Printf
open List

open Polygen_lib.Prelude
open Polygen_lib.Absyn
open Polygen_lib.Check
open Polygen_lib.Pre
open Polygen_lib.Gen
open Polygen_lib.Err
open Polygen_lib.Io


(* argument parsing *)

type mode = Check | Preprocess | Generate

let mode     = ref Generate
let times    = ref 1
let start    = ref "S"
let sources  = ref []
let dest     = ref stdout
let ps       = ref false
let seed     = ref None
let help     = ref false
let eof      = ref "\n"
let verbose  = ref false
let lbs      = ref LabelSet.empty

let specl = [
  ("-eof", Arg.String (fun s ->
                        eof :=
                          let lexbuf = Lexing.from_string ("\"" ^ s ^ "\"") in
                          try Polygen_lib.Parser.quote Polygen_lib.Lexer.token lexbuf
                          with
                          | Failure s -> arg_error (s ^ " in -eof argument")
                          | Parsing.Parse_error -> arg_error "parse error in -eof argument"
                      ),
   "STR  use string STR as end-of-file (default: STR = \"\\n\")");

  ("-help", Arg.Set help, "    display this help message");

  ("-info", Arg.Unit (fun () -> start := "I"), "    alias for '-S I'");

  ("-l", Arg.String (fun s -> lbs := LabelSet.add s !lbs), "LABEL  add LABEL to initial active label environment");

  ("-o", Arg.String (fun s -> dest := try open_out s with Sys_error _ -> io_error ("cannot open file '" ^ s ^ "' for output")),
   "DEST   output to DEST file");

  ("-pedantic", Arg.Unit (fun () -> Polygen_lib.Err.warning_level := Polygen_lib.Err.max_warning_level), "set warning level to maximum");

  ("-pre", Arg.Unit (fun () -> mode := Preprocess), "     output preprocessed source grammar");

  ("-seed", Arg.Int (fun n -> seed := Some n; Gen.do_shuffle := false), "N   pass unsigned integer N as random seed");

  ("-S", Arg.String (fun s -> start := s), "SYM    use SYM as starting non-terminal symbol (default: SYM = S)");

  ("-t", Arg.Unit (fun () -> mode := Check; Check.do_report_groups := true), "       check source grammar and output inferred global label groups");

  ("-v", Arg.Set verbose, "       be verbose");

  ("-X", Arg.Int (fun n -> times := n), "N      iterate generation for N times (default: N = 1)");

  ("-W", Arg.Int (fun n -> Polygen_lib.Err.warning_level := n), "N      set warning pedantry at level N (default: N = 1)");
]

let usage = (sprintf "Polygen (type 2) v%s build %s - http://www.polygen.org\n" Polygen_lib.Ver.ver Polygen_lib.Ver.date)
          ^ "Manta/Spinning Kids alias Alvise Spano' anno MMII ac insequenti fecit.\n\n"
          ^ "usage: polygen [OPTION]... SOURCES...\n\n"
          ^ " SOURCE     source file(s) providing grammar definition\n\n"
          ^ " OPTION"

(* auxiliary stuff *)

let msg s = if !verbose then (fprintf !dest "* %s\n" s; flush !dest) else ()

let get_decls0
    ?(msg=(fun _ -> ()))
    ?(lbs=LabelSet.empty)
    ?(start="S")
    source =
  let decls =
    msg ("loading source file \"" ^ source ^ "\"...");
    load_decls source in
  let _ = msg "checking grammar..."; Check.check lbs decls start in
  let _ = flush stderr; flush stdout in
  decls

let get_decls1
    ?(msg=(fun _ -> ()))
    ?(lbs=LabelSet.empty)
    ?(start="S")
    source =
  let decls0 = get_decls0 ~msg ~lbs ~start source in
  msg "preprocessing grammar...";
  Pre.pre decls0


(* main *)

let main seed source =
  match !mode with
  | Check -> ignore (get_decls0 ~msg ~lbs:!lbs ~start:!start source)
  | Preprocess -> fprintf !dest "%s\n" (Absyn1.pretty_decls "" (get_decls1 ~msg source))
  | Generate -> begin
    (* load complied or source grammar *)
    let decls =
      try
        let d = load_obj source in
        msg "loading compiled grammar...";
        d
      with Failure s -> msg s; get_decls1 ~msg source in

    msg ("PRNG seed: " ^ (string_of_int seed));
    msg ("EOF string: \"" ^ (String.escaped !eof) ^ "\"");
    msg ("initial label environment: " ^ LabelSet.pretty !lbs) ;
    msg ("generator output:");

    (* generate *)
    for i = 1 to !times do
      let text = Polygen_lib.generate ~lbs:!lbs ~start:!start decls in
      fprintf !dest "%s%s" text !eof
    done;

    (* store compiled grammar *)
    try
      msg "storing compiled grammar...";
      store_obj source decls
    with Failure s -> msg s
  end

let () =
  let () =
    if (Array.length Sys.argv) < 2 || !help
    then (Arg.usage specl usage; exit 0)
    else
      let anon s = sources := !sources @ [s] in
      Arg.parse specl anon usage in

  let seed = Polygen_lib.pRNG_init ~seed:!seed () in

  try
    iter (main seed) !sources;
    close_out !dest
  with Unexpected s ->
    eprintf "Unexpected termination: %s\n* Please send a bug report to the author at manta@polygen.org\n" s
