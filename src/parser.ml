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

open Parsing;;
let _ = parse_error;;
# 9 "parser.mly"
open Absyn
open Absyn.Absyn0
open List
open Prelude
open Prelude
open Parsing

let load_decls = ! Fake.load_decls


(* localize *)

let localizes n m = Err.localize (rhs_start n, rhs_end m)
let localize n = localizes n n


(* reserved symbol *)

let reserved_sym = "__R"


(* shortcuts and sugars *)

let seq a loc = (Seq (None, [(a, loc)]), loc)
let sub seqs loc = Sub (Std, [], (Prod seqs, loc))
let optsub a loc = sub [seq (Terminal Epsilon) loc; seq a loc] loc
let add_decl d (ds, p) = (d :: ds, p)
let reserved_nonterm = Fold (NonTerm (Path ([], reserved_sym)))

(* a.(l1|...|ln) ---> (X ::= a; X.l1 | ... | X.ln) *)
let multisel loc a lbs =
	let f lb = seq (Sel ((reserved_nonterm, loc), Some lb)) loc in
	let b = Bind (Def, reserved_sym, (Prod [(Seq (None, [a]), loc)], loc))
	in
    	Fold (Sub (Std, [(b, loc)], (Prod (map f lbs), loc)))

let posel loc atomss =
	let n = fold_left (fun z atoms -> max z (length atoms)) 1 atomss
	in
		if n = 1 then map hd atomss
		else
			let f n = function
				[atom] -> atom
			  | atoms  -> try nth atoms n
			  			  with _ -> Err.error loc "etherogeneous number of positional atoms in production"
			in
			let seqs = tab (fun n -> (Seq (None, map (f n) atomss), loc)) n
			in
				[(Fold (Sub (Std, [], (Prod seqs, loc))), loc)]


(* plus/minus expander *)

let expand l =
    let rec make n x = if n = 0 then [] else x :: (make (n-1) x) in
    let k =
        let l' = sort (fun (n, _) (n', _) -> compare n n') l
        in
            match l' with
                []          -> raise (Unexpected "parser.mly: expand")
              | (k, _) :: _ -> k
    in
        fold_left (fun z (n, x) -> z @ (make (n-k+1) x)) [] l


(* deep unfolder *)

let rec deep_unfold_unfoldable u =
    match u with
        NonTerm _          -> u
      | Sub (sm, decls, p) -> Sub (sm, decls, deep_unfold_prod p)

and deep_unfold_atom (a, loc) =
    match a with
        Fold u
      | Unfold u      -> (Unfold (deep_unfold_unfoldable u), loc)
      | Sel (a', lbo) -> (Sel (deep_unfold_atom a', lbo), loc)
      | _             -> (a, loc)

and deep_unfold_seq (Seq (lbo, atoms), loc) = (Seq (lbo, map deep_unfold_atom atoms), loc)

and deep_unfold_prod (Prod seqs, loc) = (Prod (map deep_unfold_seq seqs), loc)

# 122 "parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  257 (* EOL *);
  262 (* DEF *);
  263 (* ASSIGN *);
  264 (* PIPE *);
  265 (* SQBRA *);
  266 (* SQKET *);
  267 (* BRA *);
  268 (* KET *);
  269 (* GT *);
  270 (* GTGT *);
  271 (* CBRA *);
  272 (* CKET *);
  273 (* DOT *);
  274 (* PLUS *);
  275 (* MINUS *);
  276 (* COMMA *);
  277 (* COLON *);
  278 (* CAP *);
  279 (* UNDERSCORE *);
  280 (* DOTBRA *);
  281 (* BACKSLASH *);
  282 (* LTLT *);
  283 (* LT *);
  284 (* IMPORT *);
  285 (* AS *);
  286 (* SLASH *);
  287 (* STAR *);
    0|]

let yytransl_block = [|
  258 (* NONTERM *);
  259 (* TERM *);
  260 (* QUOTE *);
  261 (* DOTLABEL *);
    0|]

let yylhs = "\255\255\
\002\000\003\000\001\000\004\000\004\000\005\000\005\000\005\000\
\005\000\007\000\007\000\008\000\008\000\009\000\011\000\006\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\014\000\
\014\000\014\000\014\000\015\000\015\000\015\000\015\000\015\000\
\015\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
\019\000\019\000\020\000\020\000\021\000\021\000\012\000\012\000\
\023\000\023\000\024\000\024\000\013\000\022\000\022\000\025\000\
\025\000\026\000\026\000\016\000\017\000\017\000\017\000\017\000\
\018\000\018\000\000\000\000\000"

let yylen = "\002\000\
\001\000\001\000\002\000\002\000\003\000\003\000\003\000\002\000\
\004\000\001\000\001\000\001\000\002\000\001\000\001\000\001\000\
\001\000\001\000\002\000\002\000\002\000\002\000\004\000\001\000\
\001\000\001\000\001\000\001\000\003\000\004\000\003\000\003\000\
\003\000\001\000\005\000\005\000\005\000\005\000\004\000\004\000\
\006\000\006\000\001\000\003\000\001\000\002\000\001\000\003\000\
\001\000\002\000\001\000\003\000\001\000\001\000\001\000\001\000\
\002\000\001\000\003\000\001\000\001\000\001\000\001\000\001\000\
\001\000\003\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\067\000\000\000\000\000\
\001\000\068\000\000\000\000\000\002\000\000\000\003\000\000\000\
\000\000\000\000\062\000\000\000\000\000\000\000\000\000\000\000\
\010\000\011\000\025\000\026\000\027\000\000\000\063\000\064\000\
\006\000\000\000\000\000\000\000\014\000\049\000\015\000\016\000\
\017\000\018\000\024\000\000\000\000\000\047\000\000\000\000\000\
\053\000\007\000\000\000\005\000\000\000\000\000\034\000\000\000\
\000\000\065\000\019\000\000\000\000\000\020\000\013\000\050\000\
\022\000\021\000\000\000\000\000\000\000\061\000\046\000\000\000\
\000\000\009\000\000\000\000\000\000\000\031\000\000\000\033\000\
\032\000\044\000\054\000\055\000\000\000\000\000\056\000\000\000\
\060\000\066\000\048\000\052\000\000\000\000\000\000\000\000\000\
\030\000\057\000\023\000\000\000\000\000\000\000\034\000\040\000\
\000\000\059\000\034\000\036\000\034\000\038\000\000\000\034\000\
\042\000"

let yydgoto = "\003\000\
\006\000\010\000\014\000\007\000\008\000\055\000\034\000\035\000\
\036\000\037\000\038\000\039\000\040\000\041\000\042\000\086\000\
\043\000\044\000\056\000\045\000\046\000\047\000\048\000\049\000\
\088\000\089\000"

let yysindex = "\011\000\
\060\255\007\255\000\000\008\255\016\255\000\000\031\000\062\255\
\000\000\000\000\010\000\010\000\000\000\038\255\000\000\060\255\
\000\000\000\000\000\000\038\000\038\000\064\255\038\000\038\000\
\000\000\000\000\000\000\000\000\000\000\064\255\000\000\000\000\
\000\000\254\254\066\000\002\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\040\255\094\000\000\000\047\255\063\255\
\000\000\000\000\070\255\000\000\021\255\016\255\000\000\071\255\
\068\255\000\000\000\000\048\255\069\255\000\000\000\000\000\000\
\000\000\000\000\094\000\006\255\084\255\000\000\000\000\094\000\
\010\000\000\000\010\000\010\000\020\255\000\000\075\255\000\000\
\000\000\000\000\000\000\000\000\027\255\077\255\000\000\079\255\
\000\000\000\000\000\000\000\000\098\255\110\255\038\000\125\255\
\000\000\000\000\000\000\006\255\038\000\038\000\000\000\000\000\
\111\255\000\000\000\000\000\000\000\000\000\000\038\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\127\255\000\000\139\000\
\031\255\093\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\122\000\000\000\209\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\151\255\145\000\000\000\000\000\142\000\
\000\000\000\000\000\000\000\000\121\255\238\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\180\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\128\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\103\000\153\000\000\000\245\255\000\000\225\255\
\000\000\000\000\123\000\000\000\000\000\000\000\244\255\000\000\
\000\000\000\000\237\255\105\000\005\000\194\255\000\000\097\000\
\000\000\086\000"

let yytablesize = 427
let yytable = "\033\000\
\050\000\057\000\063\000\060\000\061\000\087\000\065\000\083\000\
\084\000\059\000\009\000\001\000\002\000\011\000\012\000\025\000\
\026\000\062\000\066\000\013\000\095\000\067\000\098\000\025\000\
\026\000\068\000\075\000\076\000\083\000\084\000\015\000\065\000\
\065\000\065\000\065\000\065\000\085\000\087\000\065\000\065\000\
\065\000\065\000\065\000\065\000\065\000\065\000\065\000\065\000\
\096\000\071\000\065\000\054\000\065\000\065\000\065\000\065\000\
\065\000\065\000\065\000\065\000\065\000\004\000\016\000\093\000\
\094\000\058\000\051\000\072\000\085\000\069\000\073\000\074\000\
\020\000\080\000\021\000\104\000\091\000\023\000\024\000\079\000\
\078\000\108\000\110\000\103\000\081\000\090\000\100\000\005\000\
\099\000\107\000\109\000\113\000\097\000\061\000\061\000\061\000\
\061\000\061\000\101\000\112\000\061\000\061\000\061\000\061\000\
\061\000\061\000\061\000\061\000\061\000\061\000\102\000\111\000\
\061\000\055\000\061\000\061\000\061\000\061\000\061\000\061\000\
\061\000\061\000\065\000\065\000\065\000\065\000\105\000\008\000\
\065\000\065\000\065\000\065\000\065\000\065\000\065\000\065\000\
\065\000\065\000\004\000\058\000\065\000\054\000\065\000\065\000\
\065\000\065\000\065\000\065\000\065\000\065\000\065\000\028\000\
\028\000\028\000\028\000\028\000\077\000\064\000\028\000\028\000\
\028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
\052\000\092\000\028\000\082\000\028\000\028\000\028\000\028\000\
\028\000\028\000\028\000\028\000\029\000\029\000\029\000\029\000\
\029\000\106\000\000\000\029\000\029\000\029\000\029\000\029\000\
\029\000\029\000\029\000\029\000\029\000\000\000\000\000\029\000\
\000\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
\029\000\043\000\043\000\043\000\043\000\000\000\000\000\000\000\
\043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
\043\000\000\000\000\000\000\000\000\000\000\000\043\000\043\000\
\000\000\043\000\043\000\043\000\043\000\043\000\000\000\063\000\
\063\000\000\000\063\000\000\000\000\000\063\000\063\000\063\000\
\063\000\063\000\063\000\063\000\063\000\063\000\063\000\000\000\
\000\000\063\000\000\000\063\000\063\000\063\000\063\000\063\000\
\063\000\063\000\063\000\017\000\018\000\019\000\000\000\000\000\
\000\000\000\000\020\000\000\000\021\000\000\000\022\000\023\000\
\024\000\000\000\000\000\025\000\026\000\000\000\000\000\027\000\
\028\000\000\000\029\000\000\000\030\000\031\000\032\000\053\000\
\018\000\019\000\000\000\000\000\000\000\000\000\020\000\000\000\
\021\000\000\000\022\000\023\000\024\000\000\000\000\000\025\000\
\026\000\000\000\000\000\027\000\028\000\000\000\029\000\000\000\
\030\000\054\000\032\000\017\000\018\000\019\000\000\000\000\000\
\000\000\000\000\020\000\000\000\021\000\000\000\022\000\023\000\
\024\000\000\000\000\000\000\000\000\000\000\000\000\000\027\000\
\028\000\000\000\029\000\000\000\030\000\031\000\032\000\058\000\
\070\000\019\000\000\000\000\000\000\000\000\000\020\000\000\000\
\021\000\000\000\022\000\023\000\024\000\000\000\000\000\000\000\
\000\000\000\000\000\000\027\000\028\000\000\000\029\000\000\000\
\030\000\031\000\032\000\012\000\012\000\012\000\000\000\000\000\
\000\000\000\000\012\000\000\000\012\000\000\000\012\000\012\000\
\012\000\000\000\000\000\000\000\000\000\000\000\051\000\012\000\
\012\000\045\000\012\000\000\000\012\000\012\000\012\000\051\000\
\045\000\051\000\045\000\000\000\045\000\051\000\000\000\000\000\
\045\000\000\000\000\000\000\000\000\000\000\000\000\000\051\000\
\000\000\000\000\045\000"

let yycheck = "\011\000\
\012\000\021\000\034\000\023\000\024\000\068\000\005\001\002\001\
\003\001\022\000\004\001\001\000\002\000\006\001\007\001\018\001\
\019\001\030\000\017\001\004\001\001\001\020\001\085\000\018\001\
\019\001\024\001\006\001\007\001\002\001\003\001\000\000\001\001\
\002\001\003\001\004\001\005\001\068\000\100\000\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\029\001\045\000\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\002\001\001\001\075\000\
\076\000\002\001\029\001\021\001\100\000\030\001\008\001\002\001\
\009\001\026\001\011\001\095\000\072\000\014\001\015\001\012\001\
\010\001\101\000\102\000\095\000\016\001\002\001\008\001\028\001\
\012\001\101\000\102\000\111\000\018\001\001\001\002\001\003\001\
\004\001\005\001\001\001\111\000\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\001\001\001\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\002\001\003\001\004\001\005\001\002\001\001\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\000\000\012\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\001\001\
\002\001\003\001\004\001\005\001\054\000\035\000\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\016\000\073\000\020\001\067\000\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\001\001\002\001\003\001\004\001\
\005\001\100\000\255\255\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\255\255\255\255\020\001\
\255\255\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\001\001\002\001\003\001\004\001\255\255\255\255\255\255\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\255\255\255\255\255\255\255\255\255\255\022\001\023\001\
\255\255\025\001\026\001\027\001\028\001\029\001\255\255\002\001\
\003\001\255\255\005\001\255\255\255\255\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\255\255\
\255\255\020\001\255\255\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\002\001\003\001\004\001\255\255\255\255\
\255\255\255\255\009\001\255\255\011\001\255\255\013\001\014\001\
\015\001\255\255\255\255\018\001\019\001\255\255\255\255\022\001\
\023\001\255\255\025\001\255\255\027\001\028\001\029\001\002\001\
\003\001\004\001\255\255\255\255\255\255\255\255\009\001\255\255\
\011\001\255\255\013\001\014\001\015\001\255\255\255\255\018\001\
\019\001\255\255\255\255\022\001\023\001\255\255\025\001\255\255\
\027\001\028\001\029\001\002\001\003\001\004\001\255\255\255\255\
\255\255\255\255\009\001\255\255\011\001\255\255\013\001\014\001\
\015\001\255\255\255\255\255\255\255\255\255\255\255\255\022\001\
\023\001\255\255\025\001\255\255\027\001\028\001\029\001\002\001\
\003\001\004\001\255\255\255\255\255\255\255\255\009\001\255\255\
\011\001\255\255\013\001\014\001\015\001\255\255\255\255\255\255\
\255\255\255\255\255\255\022\001\023\001\255\255\025\001\255\255\
\027\001\028\001\029\001\002\001\003\001\004\001\255\255\255\255\
\255\255\255\255\009\001\255\255\011\001\255\255\013\001\014\001\
\015\001\255\255\255\255\255\255\255\255\255\255\001\001\022\001\
\023\001\001\001\025\001\255\255\027\001\028\001\029\001\010\001\
\008\001\012\001\010\001\255\255\012\001\016\001\255\255\255\255\
\016\001\255\255\255\255\255\255\255\255\255\255\255\255\026\001\
\255\255\255\255\026\001"

let yynames_const = "\
  EOF\000\
  EOL\000\
  DEF\000\
  ASSIGN\000\
  PIPE\000\
  SQBRA\000\
  SQKET\000\
  BRA\000\
  KET\000\
  GT\000\
  GTGT\000\
  CBRA\000\
  CKET\000\
  DOT\000\
  PLUS\000\
  MINUS\000\
  COMMA\000\
  COLON\000\
  CAP\000\
  UNDERSCORE\000\
  DOTBRA\000\
  BACKSLASH\000\
  LTLT\000\
  LT\000\
  IMPORT\000\
  AS\000\
  SLASH\000\
  STAR\000\
  "

let yynames_block = "\
  NONTERM\000\
  TERM\000\
  QUOTE\000\
  DOTLABEL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 107 "parser.mly"
                            ( _1 )
# 404 "parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 111 "parser.mly"
           ( _1 )
# 411 "parser.ml"
               : 'file))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 115 "parser.mly"
               ( _1 )
# 418 "parser.ml"
               : Absyn0.decl list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decl) in
    Obj.repr(
# 119 "parser.mly"
                            ( [(_1, localize 1)] )
# 425 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'decl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 120 "parser.mly"
                            ( (_1, localize 1) :: _3 )
# 433 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'prod) in
    Obj.repr(
# 124 "parser.mly"
                         ( Bind (Def, _1, _3) )
# 441 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'prod) in
    Obj.repr(
# 125 "parser.mly"
                            ( Bind (Assign, _1, _3) )
# 449 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'file) in
    Obj.repr(
# 126 "parser.mly"
                            ( Import (None, load_decls _2) )
# 456 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'file) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 127 "parser.mly"
                           ( Import (Some _4, load_decls _2) )
# 464 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 133 "parser.mly"
                            ( 1 )
# 470 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 134 "parser.mly"
                            ( -1 )
# 476 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'op) in
    Obj.repr(
# 138 "parser.mly"
                            ( _1 )
# 483 "parser.ml"
               : 'modif))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'op) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'modif) in
    Obj.repr(
# 139 "parser.mly"
                            ( _1 + _2)
# 491 "parser.ml"
               : 'modif))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atom_) in
    Obj.repr(
# 143 "parser.mly"
           ( (_1, localize 1) )
# 498 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'seq_) in
    Obj.repr(
# 147 "parser.mly"
          ( (_1, localize 1) )
# 505 "parser.ml"
               : 'seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'prod_) in
    Obj.repr(
# 151 "parser.mly"
           ( (_1, localize 1) )
# 512 "parser.ml"
               : 'prod))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'terminal) in
    Obj.repr(
# 155 "parser.mly"
                            ( Terminal _1 )
# 519 "parser.ml"
               : 'atom_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unfoldable) in
    Obj.repr(
# 156 "parser.mly"
                            ( Fold _1 )
# 526 "parser.ml"
               : 'atom_))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'unfoldable) in
    Obj.repr(
# 157 "parser.mly"
                            ( Unfold _2 )
# 533 "parser.ml"
               : 'atom_))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'unfoldable) in
    Obj.repr(
# 158 "parser.mly"
                            ( Lock _2 )
# 540 "parser.ml"
               : 'atom_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    Obj.repr(
# 159 "parser.mly"
                            ( Sel (_1, None) )
# 547 "parser.ml"
               : 'atom_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 160 "parser.mly"
                            ( Sel (_1, Some _2) )
# 555 "parser.ml"
               : 'atom_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'atom) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'labels) in
    Obj.repr(
# 161 "parser.mly"
                            ( multisel (localizes 1 4) _1 _3 )
# 563 "parser.ml"
               : 'atom_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 165 "parser.mly"
                            ( Term _1 )
# 570 "parser.ml"
               : 'terminal))
; (fun __caml_parser_env ->
    Obj.repr(
# 166 "parser.mly"
                            ( Concat )
# 576 "parser.ml"
               : 'terminal))
; (fun __caml_parser_env ->
    Obj.repr(
# 167 "parser.mly"
                            ( Epsilon )
# 582 "parser.ml"
               : 'terminal))
; (fun __caml_parser_env ->
    Obj.repr(
# 168 "parser.mly"
                            ( Capitalize )
# 588 "parser.ml"
               : 'terminal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'path) in
    Obj.repr(
# 172 "parser.mly"
                            ( NonTerm (Path (tl _1, hd _1)) )
# 595 "parser.ml"
               : 'unfoldable))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'sub) in
    Obj.repr(
# 174 "parser.mly"
                            ( let (decls, p) = _2 in
                                Sub (Std, decls, p) )
# 603 "parser.ml"
               : 'unfoldable))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'sub) in
    Obj.repr(
# 178 "parser.mly"
                            ( let (decls, p) = _2 in
  							  let loc = localize 2 in
                              let sub' = (Fold (Sub (Std, decls, p)), loc) in
                              let x = reserved_nonterm in
                              let seq' = (Seq (None, [sub'; (Fold (optsub x loc), loc)]), loc) in
                              let decl = (Bind (Def, reserved_sym, (Prod [seq'], loc)), loc) in
                              let p = (Prod [seq x loc], loc)
                              in
                                  Sub (Std, [decl], p) )
# 618 "parser.ml"
               : 'unfoldable))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'sub) in
    Obj.repr(
# 189 "parser.mly"
                            ( let (decls, p) = _2 in
                                optsub (Fold (Sub (Std, decls, p))) (localize 2) )
# 626 "parser.ml"
               : 'unfoldable))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'sub) in
    Obj.repr(
# 192 "parser.mly"
                            ( let (decls, p) = _2 in
                                Sub (Mob, decls, p) )
# 634 "parser.ml"
               : 'unfoldable))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'sub) in
    Obj.repr(
# 196 "parser.mly"
                            ( let (decls, p) = _2 in
                                Sub (Std, decls, (deep_unfold_prod p)) )
# 642 "parser.ml"
               : 'unfoldable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'prod) in
    Obj.repr(
# 202 "parser.mly"
             ( ([], _1) )
# 649 "parser.ml"
               : 'sub))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'prod) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'prod) in
    Obj.repr(
# 203 "parser.mly"
                                   ( ([(Bind (Def, _1, _3), localizes 1 3)], _5) )
# 658 "parser.ml"
               : 'sub))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'prod) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'sub) in
    Obj.repr(
# 204 "parser.mly"
                                  ( add_decl (Bind (Def, _1, _3), localizes 1 3) _5 )
# 667 "parser.ml"
               : 'sub))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'prod) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'prod) in
    Obj.repr(
# 205 "parser.mly"
                                     ( ([(Bind (Assign, _1, _3), localizes 1 3)], _5) )
# 676 "parser.ml"
               : 'sub))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'prod) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'sub) in
    Obj.repr(
# 206 "parser.mly"
                                  ( add_decl (Bind (Assign, _1, _3), localizes 1 3) _5 )
# 685 "parser.ml"
               : 'sub))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'file) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'prod) in
    Obj.repr(
# 215 "parser.mly"
                              ( ([(Import (None, load_decls _2), localizes 1 2)], _4) )
# 693 "parser.ml"
               : 'sub))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'file) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'sub) in
    Obj.repr(
# 216 "parser.mly"
                              ( add_decl (Import (None, load_decls _2), localizes 1 2) _4 )
# 701 "parser.ml"
               : 'sub))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'file) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'prod) in
    Obj.repr(
# 217 "parser.mly"
                                     ( ([(Import (Some _4, load_decls _2), localizes 1 4)], _6) )
# 710 "parser.ml"
               : 'sub))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'file) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'sub) in
    Obj.repr(
# 218 "parser.mly"
                                     ( add_decl (Import (Some _4, load_decls _2), localizes 1 4) _6 )
# 719 "parser.ml"
               : 'sub))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 223 "parser.mly"
          ( [_1] )
# 726 "parser.ml"
               : 'atoms))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atom) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atoms) in
    Obj.repr(
# 224 "parser.mly"
                      ( _1 :: _3 )
# 734 "parser.ml"
               : 'atoms))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atoms) in
    Obj.repr(
# 228 "parser.mly"
                            ( [_1] )
# 741 "parser.ml"
               : 'seq0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'atoms) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'seq0) in
    Obj.repr(
# 229 "parser.mly"
                            ( _1 :: _2 )
# 749 "parser.ml"
               : 'seq0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'seq0) in
    Obj.repr(
# 233 "parser.mly"
                            ( Seq (None, posel (localize 1) _1) )
# 756 "parser.ml"
               : 'seq_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'label) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'seq0) in
    Obj.repr(
# 234 "parser.mly"
                            ( Seq (Some _1, posel (localizes 1 3) _3) )
# 764 "parser.ml"
               : 'seq_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'seq) in
    Obj.repr(
# 238 "parser.mly"
                            ( (0, _1) )
# 771 "parser.ml"
               : 'modif_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'modif) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'seq) in
    Obj.repr(
# 239 "parser.mly"
                            ( (_1, _2) )
# 779 "parser.ml"
               : 'modif_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'modif_seq) in
    Obj.repr(
# 243 "parser.mly"
                            ( [_1] )
# 786 "parser.ml"
               : 'prod0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'modif_seq) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'prod0) in
    Obj.repr(
# 244 "parser.mly"
                            ( _1 :: _3 )
# 794 "parser.ml"
               : 'prod0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'prod0) in
    Obj.repr(
# 248 "parser.mly"
                            ( Prod (expand _1) )
# 801 "parser.ml"
               : 'prod_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 252 "parser.mly"
                            ( _1 )
# 808 "parser.ml"
               : 'label))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 253 "parser.mly"
                            ( _1 )
# 815 "parser.ml"
               : 'label))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'label) in
    Obj.repr(
# 257 "parser.mly"
                            ( (0, _1) )
# 822 "parser.ml"
               : 'modif_label))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'modif) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'label) in
    Obj.repr(
# 258 "parser.mly"
                            ( (_1, _2) )
# 830 "parser.ml"
               : 'modif_label))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'modif_label) in
    Obj.repr(
# 262 "parser.mly"
                                  ( [_1] )
# 837 "parser.ml"
               : 'multilabels))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'modif_label) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'multilabels) in
    Obj.repr(
# 263 "parser.mly"
                                  ( _1 :: _3 )
# 845 "parser.ml"
               : 'multilabels))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'multilabels) in
    Obj.repr(
# 267 "parser.mly"
                            ( expand _1 )
# 852 "parser.ml"
               : 'labels))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 271 "parser.mly"
                            ( _1 )
# 859 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 272 "parser.mly"
                            ( _1 )
# 866 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 273 "parser.mly"
                            ( "import" )
# 872 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 274 "parser.mly"
                            ( "as" )
# 878 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 278 "parser.mly"
                            ( [_1] )
# 885 "parser.ml"
               : 'path))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'path) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 279 "parser.mly"
                            ( _3 :: _1 )
# 893 "parser.ml"
               : 'path))
(* Entry source *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry quote *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let source (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Absyn0.decl list)
let quote (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : string)
;;
# 283 "parser.mly"

# 924 "parser.ml"
