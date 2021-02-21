(*
 * Polygen
 * main.ml: main code
 *
 * Alvise Spano' (2002-03)
 *)

open Printf
open List

open Prelude
open Prelude
open Absyn
open Check
open Pre
open Gen
open Err
open Io


(* argument parsing *)

type mode = Check | Preprocess | Generate

let mode     = ref Generate
let times    = ref 1
let start    = ref "S"
let sources  = ref []
let dest     = ref stdout
let ps       = ref false
let help     = ref false
let eof      = ref "\n"
let verbose  = ref false
let lbs      = ref LabelSet.empty

let specl = [
  ("-eof", Arg.String (fun s ->
                        eof :=
                          let lexbuf = Lexing.from_string ("\"" ^ s ^ "\"") in
                          try Parser.quote Lexer.token lexbuf
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

  ("-pedantic", Arg.Unit (fun () -> Err.warning_level := Err.max_warning_level), "set warning level to maximum");

  ("-pre", Arg.Unit (fun () -> mode := Preprocess), "     output preprocessed source grammar");

  ("-seed", Arg.Int (fun n -> Polygen_lib.seed := Some n; Gen.do_shuffle := false), "N   pass unsigned integer N as random seed");

  ("-S", Arg.String (fun s -> start := s), "SYM    use SYM as starting non-terminal symbol (default: SYM = S)");

  ("-t", Arg.Unit (fun () -> mode := Check; Check.do_report_groups := true), "       check source grammar and output inferred global label groups");

  ("-v", Arg.Set verbose, "       be verbose");

  ("-X", Arg.Int (fun n -> times := n), "N      iterate generation for N times (default: N = 1)");

  ("-W", Arg.Int (fun n -> Err.warning_level := n), "N      set warning pedantry at level N (default: N = 1)");
]

let usage = (sprintf "Polygen (type 2) v%s build %s - http://www.polygen.org\n" Ver.ver Ver.date)
          ^ "Manta/Spinning Kids alias Alvise Spano' anno MMII ac insequenti fecit.\n\n"
          ^ "usage: polygen [OPTION]... SOURCES...\n\n"
          ^ " SOURCE     source file(s) providing grammar definition\n\n"
          ^ " OPTION"

(* auxiliary stuff *)

let msg s = if !verbose then (fprintf !dest "* %s\n" s; flush !dest) else ()

(* main *)

let main source =
  match !mode with
  | Check -> ignore (Polygen_lib.get_decls0 ~msg ~lbs:!lbs ~start:!start source)
  | Preprocess -> fprintf !dest "%s\n" (Absyn1.pretty_decls "" (Polygen_lib.get_decls1 ~msg source))
  | Generate -> Polygen_lib.generate ~msg ~eof:!eof ~lbs:!lbs ~start:!start ~times:!times ~dest:!dest source

let () =
  Polygen_lib.pRNG_init ();

  let () =
    if (Array.length Sys.argv) < 2 || !help
    then (Arg.usage specl usage; exit 0)
    else
      let anon s = sources := !sources @ [s] in
      Arg.parse specl anon usage in

  try
    iter main !sources;
    close_out !dest
  with Unexpected s ->
    eprintf "Unexpected termination: %s\n* Please send a bug report to the author at manta@polygen.org\n" s
