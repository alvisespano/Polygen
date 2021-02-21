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

type declarations = Absyn.Absyn1.decl list

val pRNG_init : ?seed:int option -> unit -> int

val load_decls :
  string -> (declarations, string) result

val generate :
  ?lbs:Prelude.LabelSet.t ->
  ?start:Prelude.symbol -> declarations -> string
