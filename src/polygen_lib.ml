module Prelude = Prelude
module Absyn = Absyn
module Check = Check
module Pre = Pre
module Gen = Gen
module Err = Err
module Io = Io
module Parser = Parser
module Lexer = Lexer
module Ver = Ver

open Prelude
open Gen
open Err
open Io

type declarations = Absyn.Absyn1.decl list

let pRNG_init ?(seed=None) () =
  match seed with
  | None   -> (Random.self_init (); Random.bits ())
  | Some s -> (Random.init s; s)

let uerror s = Error (Printf.sprintf "error: %s\n" s)
let error loc s = uerror (localized_prompt s loc)
let syntax_error file lexbuf s =
  error (localize (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)) s

let load_decls grammar =
  try
    let _ = jump "stdin" in
    let lexbuf = Lexing.from_string grammar in
    try
      let decls = Parser.source Lexer.token lexbuf in
      let _ = jump_back () in
      Ok (Pre.Pre.pre decls)
    with
    | Failure s -> syntax_error "stdin" lexbuf s

    |  Parsing.Parse_error ->
      let s = String.escaped (Lexing.lexeme lexbuf) in
      let unexpected = (if s = "" then "empty token" else "token \"" ^ s ^ "\"") in
      syntax_error "stdin" lexbuf ("unexpected " ^ unexpected)
  with
  | Sys_error s -> uerror ("cannot parse from file \"" ^ s ^ "\"")

let generate
    ?(lbs=LabelSet.empty)
    ?(start="S")
    decls =
  Gen.gen lbs decls start
