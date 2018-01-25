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
# 8 "parser.mly"

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

# 123 "parser.ml"
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
\007\000\007\000\008\000\008\000\009\000\011\000\006\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\014\000\014\000\
\014\000\014\000\015\000\015\000\015\000\015\000\015\000\015\000\
\019\000\019\000\019\000\019\000\019\000\019\000\019\000\020\000\
\020\000\021\000\021\000\012\000\012\000\023\000\023\000\024\000\
\024\000\013\000\022\000\022\000\025\000\025\000\026\000\026\000\
\016\000\017\000\017\000\017\000\017\000\018\000\018\000\000\000\
\000\000"

let yylen = "\002\000\
\001\000\001\000\002\000\002\000\003\000\003\000\003\000\004\000\
\001\000\001\000\001\000\002\000\001\000\001\000\001\000\001\000\
\001\000\002\000\002\000\002\000\002\000\004\000\001\000\001\000\
\001\000\001\000\001\000\003\000\004\000\003\000\003\000\003\000\
\001\000\005\000\005\000\005\000\005\000\006\000\006\000\001\000\
\003\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\
\003\000\001\000\001\000\001\000\001\000\002\000\001\000\003\000\
\001\000\001\000\001\000\001\000\001\000\001\000\003\000\002\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\064\000\000\000\000\000\
\001\000\065\000\000\000\000\000\002\000\000\000\003\000\000\000\
\000\000\000\000\059\000\000\000\000\000\000\000\000\000\000\000\
\009\000\010\000\024\000\025\000\026\000\000\000\060\000\061\000\
\006\000\000\000\000\000\000\000\013\000\046\000\014\000\015\000\
\016\000\017\000\023\000\000\000\000\000\044\000\000\000\000\000\
\050\000\007\000\000\000\005\000\000\000\000\000\033\000\000\000\
\000\000\062\000\018\000\000\000\000\000\019\000\012\000\047\000\
\021\000\020\000\000\000\000\000\000\000\058\000\043\000\000\000\
\000\000\008\000\000\000\000\000\000\000\030\000\000\000\032\000\
\031\000\041\000\051\000\052\000\000\000\000\000\053\000\000\000\
\057\000\063\000\045\000\049\000\000\000\000\000\000\000\029\000\
\054\000\022\000\000\000\000\000\000\000\000\000\056\000\033\000\
\035\000\033\000\037\000\000\000\033\000\039\000"

let yydgoto = "\003\000\
\006\000\010\000\014\000\007\000\008\000\055\000\034\000\035\000\
\036\000\037\000\038\000\039\000\040\000\041\000\042\000\086\000\
\043\000\044\000\056\000\045\000\046\000\047\000\048\000\049\000\
\088\000\089\000"

let yysindex = "\009\000\
\007\255\004\255\000\000\008\255\018\255\000\000\012\000\032\255\
\000\000\000\000\235\255\235\255\000\000\010\255\000\000\007\255\
\000\000\000\000\000\000\007\000\007\000\127\000\007\000\007\000\
\000\000\000\000\000\000\000\000\000\000\127\000\000\000\000\000\
\000\000\027\255\035\000\223\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\048\255\063\000\000\000\058\255\072\255\
\000\000\000\000\079\255\000\000\070\255\018\255\000\000\075\255\
\074\255\000\000\000\000\056\255\071\255\000\000\000\000\000\000\
\000\000\000\000\063\000\137\255\086\255\000\000\000\000\063\000\
\235\255\000\000\235\255\235\255\069\255\000\000\092\255\000\000\
\000\000\000\000\000\000\000\000\050\255\097\255\000\000\119\255\
\000\000\000\000\000\000\000\000\127\255\166\255\183\255\000\000\
\000\000\000\000\137\255\007\000\007\000\167\255\000\000\000\000\
\000\000\000\000\000\000\007\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\186\000\
\091\255\121\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\091\000\000\000\015\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\046\255\114\000\000\000\000\000\111\000\
\000\000\000\000\000\000\000\000\149\255\207\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\179\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\185\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\144\000\184\000\000\000\245\255\000\000\224\255\
\000\000\000\000\176\000\000\000\000\000\000\000\247\255\000\000\
\000\000\000\000\239\255\146\000\216\255\191\255\000\000\141\000\
\000\000\126\000"

let yytablesize = 398
let yytable = "\033\000\
\050\000\063\000\087\000\057\000\071\000\060\000\061\000\009\000\
\004\000\001\000\002\000\015\000\059\000\011\000\012\000\040\000\
\040\000\040\000\040\000\097\000\062\000\013\000\040\000\040\000\
\040\000\040\000\040\000\040\000\040\000\040\000\040\000\091\000\
\016\000\087\000\005\000\085\000\040\000\040\000\051\000\040\000\
\040\000\040\000\040\000\040\000\025\000\026\000\027\000\027\000\
\027\000\027\000\027\000\083\000\084\000\027\000\027\000\027\000\
\027\000\027\000\027\000\027\000\027\000\027\000\027\000\093\000\
\094\000\027\000\085\000\027\000\027\000\027\000\027\000\027\000\
\027\000\027\000\027\000\075\000\076\000\069\000\072\000\073\000\
\074\000\080\000\105\000\107\000\078\000\079\000\081\000\090\000\
\104\000\106\000\110\000\062\000\062\000\062\000\062\000\062\000\
\109\000\095\000\062\000\062\000\062\000\062\000\062\000\062\000\
\062\000\062\000\062\000\062\000\098\000\096\000\062\000\051\000\
\062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
\062\000\058\000\058\000\058\000\058\000\058\000\099\000\100\000\
\058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
\058\000\058\000\083\000\084\000\058\000\052\000\058\000\058\000\
\058\000\058\000\058\000\058\000\058\000\058\000\062\000\062\000\
\062\000\062\000\025\000\026\000\062\000\062\000\062\000\062\000\
\062\000\062\000\062\000\062\000\062\000\062\000\101\000\108\000\
\062\000\051\000\062\000\062\000\062\000\062\000\062\000\062\000\
\062\000\062\000\062\000\028\000\028\000\028\000\028\000\028\000\
\102\000\004\000\028\000\028\000\028\000\028\000\028\000\028\000\
\028\000\028\000\028\000\028\000\055\000\077\000\028\000\052\000\
\028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
\060\000\060\000\064\000\060\000\082\000\092\000\060\000\060\000\
\060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
\103\000\000\000\060\000\065\000\060\000\060\000\060\000\060\000\
\060\000\060\000\060\000\060\000\017\000\018\000\019\000\066\000\
\000\000\000\000\067\000\020\000\000\000\021\000\068\000\022\000\
\023\000\024\000\000\000\000\000\025\000\026\000\000\000\000\000\
\027\000\028\000\000\000\029\000\000\000\030\000\031\000\032\000\
\053\000\018\000\019\000\000\000\000\000\000\000\000\000\020\000\
\000\000\021\000\000\000\022\000\023\000\024\000\000\000\000\000\
\025\000\026\000\000\000\000\000\027\000\028\000\000\000\029\000\
\000\000\030\000\054\000\032\000\017\000\018\000\019\000\000\000\
\000\000\000\000\000\000\020\000\000\000\021\000\000\000\022\000\
\023\000\024\000\000\000\000\000\000\000\000\000\000\000\000\000\
\027\000\028\000\000\000\029\000\000\000\030\000\031\000\032\000\
\058\000\070\000\019\000\000\000\000\000\000\000\000\000\020\000\
\000\000\021\000\000\000\022\000\023\000\024\000\000\000\000\000\
\000\000\000\000\000\000\000\000\027\000\028\000\000\000\029\000\
\000\000\030\000\031\000\032\000\011\000\011\000\011\000\000\000\
\000\000\000\000\000\000\011\000\000\000\011\000\000\000\011\000\
\011\000\011\000\000\000\000\000\000\000\000\000\000\000\048\000\
\011\000\011\000\042\000\011\000\000\000\011\000\011\000\011\000\
\048\000\042\000\048\000\042\000\000\000\042\000\048\000\000\000\
\058\000\042\000\000\000\000\000\000\000\000\000\000\000\020\000\
\048\000\021\000\000\000\042\000\023\000\024\000"

let yycheck = "\011\000\
\012\000\034\000\068\000\021\000\045\000\023\000\024\000\004\001\
\002\001\001\000\002\000\000\000\022\000\006\001\007\001\001\001\
\002\001\003\001\004\001\085\000\030\000\004\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\072\000\
\001\001\099\000\028\001\068\000\022\001\023\001\029\001\025\001\
\026\001\027\001\028\001\029\001\018\001\019\001\001\001\002\001\
\003\001\004\001\005\001\002\001\003\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\075\000\
\076\000\020\001\099\000\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\006\001\007\001\030\001\021\001\008\001\
\002\001\026\001\100\000\101\000\010\001\012\001\016\001\002\001\
\100\000\101\000\108\000\001\001\002\001\003\001\004\001\005\001\
\108\000\029\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\012\001\018\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\001\001\002\001\003\001\004\001\005\001\008\001\001\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\002\001\003\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\002\001\003\001\
\004\001\005\001\018\001\019\001\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\001\001\001\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\001\001\002\001\003\001\004\001\005\001\
\002\001\000\000\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\012\001\054\000\020\001\016\000\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\002\001\003\001\035\000\005\001\067\000\073\000\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\099\000\255\255\020\001\005\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\002\001\003\001\004\001\017\001\
\255\255\255\255\020\001\009\001\255\255\011\001\024\001\013\001\
\014\001\015\001\255\255\255\255\018\001\019\001\255\255\255\255\
\022\001\023\001\255\255\025\001\255\255\027\001\028\001\029\001\
\002\001\003\001\004\001\255\255\255\255\255\255\255\255\009\001\
\255\255\011\001\255\255\013\001\014\001\015\001\255\255\255\255\
\018\001\019\001\255\255\255\255\022\001\023\001\255\255\025\001\
\255\255\027\001\028\001\029\001\002\001\003\001\004\001\255\255\
\255\255\255\255\255\255\009\001\255\255\011\001\255\255\013\001\
\014\001\015\001\255\255\255\255\255\255\255\255\255\255\255\255\
\022\001\023\001\255\255\025\001\255\255\027\001\028\001\029\001\
\002\001\003\001\004\001\255\255\255\255\255\255\255\255\009\001\
\255\255\011\001\255\255\013\001\014\001\015\001\255\255\255\255\
\255\255\255\255\255\255\255\255\022\001\023\001\255\255\025\001\
\255\255\027\001\028\001\029\001\002\001\003\001\004\001\255\255\
\255\255\255\255\255\255\009\001\255\255\011\001\255\255\013\001\
\014\001\015\001\255\255\255\255\255\255\255\255\255\255\001\001\
\022\001\023\001\001\001\025\001\255\255\027\001\028\001\029\001\
\010\001\008\001\012\001\010\001\255\255\012\001\016\001\255\255\
\002\001\016\001\255\255\255\255\255\255\255\255\255\255\009\001\
\026\001\011\001\255\255\026\001\014\001\015\001"

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
# 394 "parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 111 "parser.mly"
           ( _1 )
# 401 "parser.ml"
               : 'file))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 115 "parser.mly"
               ( _1 )
# 408 "parser.ml"
               : Absyn0.decl list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decl) in
    Obj.repr(
# 119 "parser.mly"
                            ( [(_1, localize 1)] )
# 415 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'decl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 120 "parser.mly"
                            ( (_1, localize 1) :: _3 )
# 423 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'prod) in
    Obj.repr(
# 124 "parser.mly"
                         ( Bind (Def, _1, _3) )
# 431 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'prod) in
    Obj.repr(
# 125 "parser.mly"
                            ( Bind (Assign, _1, _3) )
# 439 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'file) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 126 "parser.mly"
                           ( Import (_4, load_decls _2) )
# 447 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 130 "parser.mly"
                            ( 1 )
# 453 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 131 "parser.mly"
                            ( -1 )
# 459 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'op) in
    Obj.repr(
# 135 "parser.mly"
                            ( _1 )
# 466 "parser.ml"
               : 'modif))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'op) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'modif) in
    Obj.repr(
# 136 "parser.mly"
                            ( _1 + _2)
# 474 "parser.ml"
               : 'modif))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atom_) in
    Obj.repr(
# 140 "parser.mly"
           ( (_1, localize 1) )
# 481 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'seq_) in
    Obj.repr(
# 144 "parser.mly"
          ( (_1, localize 1) )
# 488 "parser.ml"
               : 'seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'prod_) in
    Obj.repr(
# 148 "parser.mly"
           ( (_1, localize 1) )
# 495 "parser.ml"
               : 'prod))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'terminal) in
    Obj.repr(
# 152 "parser.mly"
                            ( Terminal _1 )
# 502 "parser.ml"
               : 'atom_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unfoldable) in
    Obj.repr(
# 153 "parser.mly"
                            ( Fold _1 )
# 509 "parser.ml"
               : 'atom_))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'unfoldable) in
    Obj.repr(
# 154 "parser.mly"
                            ( Unfold _2 )
# 516 "parser.ml"
               : 'atom_))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'unfoldable) in
    Obj.repr(
# 155 "parser.mly"
                            ( Lock _2 )
# 523 "parser.ml"
               : 'atom_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    Obj.repr(
# 156 "parser.mly"
                            ( Sel (_1, None) )
# 530 "parser.ml"
               : 'atom_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 157 "parser.mly"
                            ( Sel (_1, Some _2) )
# 538 "parser.ml"
               : 'atom_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'atom) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'labels) in
    Obj.repr(
# 158 "parser.mly"
                            ( multisel (localizes 1 4) _1 _3 )
# 546 "parser.ml"
               : 'atom_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 162 "parser.mly"
                            ( Term _1 )
# 553 "parser.ml"
               : 'terminal))
; (fun __caml_parser_env ->
    Obj.repr(
# 163 "parser.mly"
                            ( Concat )
# 559 "parser.ml"
               : 'terminal))
; (fun __caml_parser_env ->
    Obj.repr(
# 164 "parser.mly"
                            ( Epsilon )
# 565 "parser.ml"
               : 'terminal))
; (fun __caml_parser_env ->
    Obj.repr(
# 165 "parser.mly"
                            ( Capitalize )
# 571 "parser.ml"
               : 'terminal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'path) in
    Obj.repr(
# 169 "parser.mly"
                            ( NonTerm (Path (tl _1, hd _1)) )
# 578 "parser.ml"
               : 'unfoldable))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'sub) in
    Obj.repr(
# 171 "parser.mly"
                            ( let (decls, p) = _2 in
                                Sub (Std, decls, p) )
# 586 "parser.ml"
               : 'unfoldable))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'sub) in
    Obj.repr(
# 175 "parser.mly"
                            ( let (decls, p) = _2 in
  							  let loc = localize 2 in
                              let sub' = (Fold (Sub (Std, decls, p)), loc) in
                              let x = reserved_nonterm in
                              let seq' = (Seq (None, [sub'; (Fold (optsub x loc), loc)]), loc) in
                              let decl = (Bind (Def, reserved_sym, (Prod [seq'], loc)), loc) in
                              let p = (Prod [seq x loc], loc)
                              in
                                  Sub (Std, [decl], p) )
# 601 "parser.ml"
               : 'unfoldable))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'sub) in
    Obj.repr(
# 186 "parser.mly"
                            ( let (decls, p) = _2 in
                                optsub (Fold (Sub (Std, decls, p))) (localize 2) )
# 609 "parser.ml"
               : 'unfoldable))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'sub) in
    Obj.repr(
# 189 "parser.mly"
                            ( let (decls, p) = _2 in
                                Sub (Mob, decls, p) )
# 617 "parser.ml"
               : 'unfoldable))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'sub) in
    Obj.repr(
# 193 "parser.mly"
                            ( let (decls, p) = _2 in
                                Sub (Std, decls, (deep_unfold_prod p)) )
# 625 "parser.ml"
               : 'unfoldable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'prod) in
    Obj.repr(
# 199 "parser.mly"
             ( ([], _1) )
# 632 "parser.ml"
               : 'sub))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'prod) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'prod) in
    Obj.repr(
# 200 "parser.mly"
                                   ( ([(Bind (Def, _1, _3), localizes 1 3)], _5) )
# 641 "parser.ml"
               : 'sub))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'prod) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'sub) in
    Obj.repr(
# 201 "parser.mly"
                                  ( add_decl (Bind (Def, _1, _3), localizes 1 3) _5 )
# 650 "parser.ml"
               : 'sub))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'prod) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'prod) in
    Obj.repr(
# 202 "parser.mly"
                                     ( ([(Bind (Assign, _1, _3), localizes 1 3)], _5) )
# 659 "parser.ml"
               : 'sub))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'prod) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'sub) in
    Obj.repr(
# 203 "parser.mly"
                                    ( add_decl (Bind (Assign, _1, _3), localizes 1 3) _5 )
# 668 "parser.ml"
               : 'sub))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'file) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'prod) in
    Obj.repr(
# 204 "parser.mly"
                                     ( ([(Import (_4, load_decls _2), localizes 1 4)], _6) )
# 677 "parser.ml"
               : 'sub))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'file) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'sub) in
    Obj.repr(
# 205 "parser.mly"
                                     ( add_decl (Import (_4, load_decls _2), localizes 1 4) _6 )
# 686 "parser.ml"
               : 'sub))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 209 "parser.mly"
          ( [_1] )
# 693 "parser.ml"
               : 'atoms))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atom) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atoms) in
    Obj.repr(
# 210 "parser.mly"
                      ( _1 :: _3 )
# 701 "parser.ml"
               : 'atoms))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atoms) in
    Obj.repr(
# 214 "parser.mly"
                            ( [_1] )
# 708 "parser.ml"
               : 'seq0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'atoms) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'seq0) in
    Obj.repr(
# 215 "parser.mly"
                            ( _1 :: _2 )
# 716 "parser.ml"
               : 'seq0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'seq0) in
    Obj.repr(
# 219 "parser.mly"
                            ( Seq (None, posel (localize 1) _1) )
# 723 "parser.ml"
               : 'seq_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'label) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'seq0) in
    Obj.repr(
# 220 "parser.mly"
                            ( Seq (Some _1, posel (localizes 1 3) _3) )
# 731 "parser.ml"
               : 'seq_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'seq) in
    Obj.repr(
# 224 "parser.mly"
                            ( (0, _1) )
# 738 "parser.ml"
               : 'modif_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'modif) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'seq) in
    Obj.repr(
# 225 "parser.mly"
                            ( (_1, _2) )
# 746 "parser.ml"
               : 'modif_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'modif_seq) in
    Obj.repr(
# 229 "parser.mly"
                            ( [_1] )
# 753 "parser.ml"
               : 'prod0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'modif_seq) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'prod0) in
    Obj.repr(
# 230 "parser.mly"
                            ( _1 :: _3 )
# 761 "parser.ml"
               : 'prod0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'prod0) in
    Obj.repr(
# 234 "parser.mly"
                            ( Prod (expand _1) )
# 768 "parser.ml"
               : 'prod_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 238 "parser.mly"
                            ( _1 )
# 775 "parser.ml"
               : 'label))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 239 "parser.mly"
                            ( _1 )
# 782 "parser.ml"
               : 'label))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'label) in
    Obj.repr(
# 243 "parser.mly"
                            ( (0, _1) )
# 789 "parser.ml"
               : 'modif_label))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'modif) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'label) in
    Obj.repr(
# 244 "parser.mly"
                            ( (_1, _2) )
# 797 "parser.ml"
               : 'modif_label))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'modif_label) in
    Obj.repr(
# 248 "parser.mly"
                                  ( [_1] )
# 804 "parser.ml"
               : 'multilabels))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'modif_label) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'multilabels) in
    Obj.repr(
# 249 "parser.mly"
                                  ( _1 :: _3 )
# 812 "parser.ml"
               : 'multilabels))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'multilabels) in
    Obj.repr(
# 253 "parser.mly"
                            ( expand _1 )
# 819 "parser.ml"
               : 'labels))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 257 "parser.mly"
                            ( _1 )
# 826 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 258 "parser.mly"
                            ( _1 )
# 833 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 259 "parser.mly"
                            ( "import" )
# 839 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 260 "parser.mly"
                            ( "as" )
# 845 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 264 "parser.mly"
                            ( [_1] )
# 852 "parser.ml"
               : 'path))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'path) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 265 "parser.mly"
                            ( _3 :: _1 )
# 860 "parser.ml"
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
# 268 "parser.mly"


# 892 "parser.ml"
