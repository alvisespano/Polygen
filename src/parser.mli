open Absyn

type token =
  | EOF
  | EOL
  | NONTERM of (string)
  | TERM of (string)
  | QUOTE of (string)
  | DOTLABEL of (string)
  | DEF
  | ASSIGN
  | PIPE
  | SQBRA
  | SQKET
  | BRA
  | KET
  | GT
  | GTGT
  | CBRA
  | CKET
  | DOT
  | PLUS
  | MINUS
  | COMMA
  | COLON
  | CAP
  | UNDERSCORE
  | DOTBRA
  | BACKSLASH
  | LTLT
  | LT
  | IMPORT
  | AS
  | SLASH
  | STAR

val source :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Absyn0.decl list
val quote :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string
