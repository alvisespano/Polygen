%{

(*
 * Polygen
 * parser.mly: parser definition for Yacc
 *
 * (C) 2002-2018 Alvise Spano'
 *)

open Absyn
open Absyn.Absyn0
open Prelude
open Parsing
open ParserAux

let load_decls = !Fake.load_decls

%}

%token EOF EOL
%token <string> NONTERM TERM QUOTE DOTLABEL
%token DEF ASSIGN PIPE SQBRA SQKET BRA KET GT GTGT CBRA CKET DOT PLUS MINUS COMMA
       COLON CAP UNDERSCORE DOTBRA BACKSLASH LTLT LT IMPORT AS SLASH STAR

%start source
%type <Absyn.Absyn0.decl list> source
%start quote
%type <string> quote

%%

quote:
    QUOTE                   { $1 }
;

file:
	QUOTE					{ $1 }
;

source:
	decls EOF 				{ $1 }
;

decls:
    decl EOL                { [($1, localize 1)] }
  | decl EOL decls          { ($1, localize 1) :: $3 }
;

decl:
	NONTERM DEF prod        { Bind (Def, $1, $3) }
  | NONTERM ASSIGN prod     { Bind (Assign, $1, $3) }
  | IMPORT file             { Import (None, load_decls $2) }
  | IMPORT file AS NONTERM	{ Import (Some $4, load_decls $2) }
/*| IMPORT file 			{ Import ($2, None) }
  | IMPORT file AS NONTERM	{ Import ($2, Some $4) }*/
;

op:
    PLUS                    { 1 }
  | MINUS                   { -1 }
;

modif:
    op                      { $1 }
  | op modif                { $1 + $2}
;

atom:
	atom_					{ ($1, localize 1) }
;

seq:
	seq_					{ ($1, localize 1) }
;

prod:
	prod_					{ ($1, localize 1) }
;

atom_:
    terminal                { Terminal $1 }
  | unfoldable              { Fold $1 }
  | GT unfoldable           { Unfold $2 }
  | LT unfoldable           { Lock $2 }
  | atom DOT                { Sel ($1, None) }
  | atom DOTLABEL           { Sel ($1, Some $2) }
  | atom DOTBRA labels KET  { multisel (localizes 1 4) $1 $3 }
;

terminal:
    term                    { Term $1 }
  | CAP                     { Concat }
  | UNDERSCORE              { Epsilon }
  | BACKSLASH               { Capitalize }
;

unfoldable:
    path                    { NonTerm (Path (List.tl $1, List.hd $1)) }

  | BRA sub KET             { let (decls, p) = $2 in
                                Sub (Std, decls, p) }

  /*  (D; P)+ ---> (X ::= (D; P) (_ | X); X) */
  | BRA sub KET PLUS        { let (decls, p) = $2 in
  							  let loc = localize 2 in
                              let sub' = (Fold (Sub (Std, decls, p)), loc) in
                              let x = reserved_nonterm in
                              let seq' = (Seq (None, [sub'; (Fold (optsub x loc), loc)]), loc) in
                              let decl = (Bind (Def, reserved_sym, (Prod [seq'], loc)), loc) in
                              let p = (Prod [seq x loc], loc)
                              in
                                  Sub (Std, [decl], p) }

  /* [D; P] ---> ((D; P) | _) */
  | SQBRA sub SQKET         { let (decls, p) = $2 in
                                optsub (Fold (Sub (Std, decls, p))) (localize 2) }

  | CBRA sub CKET           { let (decls, p) = $2 in
                                Sub (Mob, decls, p) }

  /* >> D; P << ---> (D; >> P <<) */
  | GTGT sub LTLT           { let (decls, p) = $2 in
                                Sub (Std, decls, (deep_unfold_prod p)) }
;

/* the following replication is needed for making Yacc perform look-ahead! */
sub:
	prod								{ ([], $1) }
  |	NONTERM DEF prod EOL prod    		{ ([(Bind (Def, $1, $3), localizes 1 3)], $5) }
  | NONTERM DEF prod EOL sub    		{ add_decl (Bind (Def, $1, $3), localizes 1 3) $5 }
  |	NONTERM ASSIGN prod EOL prod   		{ ([(Bind (Assign, $1, $3), localizes 1 3)], $5) }
  | NONTERM ASSIGN prod EOL sub			{ add_decl (Bind (Assign, $1, $3), localizes 1 3) $5 }

  /* new productions for import, based on simple AST and deferred loading of grammars */
/*| IMPORT file EOL prod  				{ ([(Import ($2, None), localizes 1 2)], $4) }
  | IMPORT file EOL sub   				{ add_decl (Import ($2, None), localizes 1 2) $4 }
  | IMPORT file AS NONTERM EOL prod  	{ ([(Import ($2, Some $4), localizes 1 4)], $6) }
  | IMPORT file AS NONTERM EOL sub   	{ add_decl (Import ($2, Some $4), localizes 1 4) $6 }*/

  /* old productions for import, based on loading grammars at parse-time */
  | IMPORT file EOL prod  				{ ([(Import (None, load_decls $2), localizes 1 2)], $4) }
  | IMPORT file EOL sub   				{ add_decl (Import (None, load_decls $2), localizes 1 2) $4 }
  | IMPORT file AS NONTERM EOL prod 	{ ([(Import (Some $4, load_decls $2), localizes 1 4)], $6) }
  | IMPORT file AS NONTERM EOL sub  	{ add_decl (Import (Some $4, load_decls $2), localizes 1 4) $6 }
  
;

atoms:
	atom					{ [$1] }
  | atom COMMA atoms		{ $1 :: $3 }
;

seq0:
    atoms                   { [$1] }
  | atoms seq0              { $1 :: $2 }
;

seq_:
    seq0                    { Seq (None, posel (localize 1) $1) }
  | label COLON seq0       	{ Seq (Some $1, posel (localizes 1 3) $3) }
;

modif_seq:
    seq                    	{ (0, $1) }
  | modif seq              	{ ($1, $2) }
;

prod0:
    modif_seq               { [$1] }
  | modif_seq PIPE prod0    { $1 :: $3 }
;

prod_:
    prod0                   { Prod (expand $1) }
;

label:
    NONTERM                 { $1 }
  | TERM                    { $1 }
;

modif_label:
    label                  	{ (0, $1) }
  | modif label            	{ ($1, $2) }
;

multilabels:
    modif_label                   { [$1] }
  | modif_label PIPE multilabels  { $1 :: $3 }
;

labels:
    multilabels             { expand $1 }
;

term:
    TERM                    { $1 }
  | QUOTE                   { $1 }
  | IMPORT                  { "import" }
  | AS                      { "as" }
;

path:
    NONTERM                 { [$1] }
  | path SLASH NONTERM      { $3 :: $1 }
;

%%

