(*
 * Polygen
 * absyn.ml: abstract syntax definition
 *
 * Alvise Spano' (2002)
 *)

open Prelude

(* path type for qualified access of imported non-terminal symbols *)

type path = Path of symbol list * symbol

module Path =
  struct
    type t = path

    let of_sym sym = Path ([], sym)

    let pretty (Path (ps, sym)) =
        match ps with
        | [] -> sym
        | _  -> (flatten_strings "/" ps) ^ "/" ^ sym

    let append sym' (Path (syms, sym)) = Path (syms @ [sym], sym')
  end


module Absyn0 =
  struct
  	type sub_mode = Std | Mob

  	type bind_mode = Def | Assign

    type atom' =
        Terminal of terminal
      | Sel of atom * symbol option
      | Fold of unfoldable
      | Unfold of unfoldable
      | Lock of unfoldable

    and terminal =
        Epsilon
      | Concat
      | Capitalize
      | Term of symbol

    and unfoldable =
        NonTerm of path
      | Sub of sub_mode * decl list * prod

    and seq' = Seq of symbol option * atom list

    and prod' = Prod of seq list

    and decl' = Bind of bind_mode * symbol * prod
              | Import of symbol option * decl list
			  
    and atom = atom' * Err.loc
    and seq = seq' * Err.loc
    and prod = prod' * Err.loc
    and decl = decl' * Err.loc

  end

module Absyn1 =
  struct
  	type bind_mode = Def | Assign

    type atom =
        Terminal of terminal
      | NonTerm of path
      | Sel of atom * symbol option
      | Sub of decl list * prod

    and terminal =
        Epsilon
      | Concat
      | Capitalize
      | Term of symbol

    and seq = Seq of symbol option * atom list * int ref

    and prod = Prod of seq list

    and decl = Bind of bind_mode * symbol * prod


    (* pretty stringing *)

    let tab ind s = ind ^ (String.make (String.length s) ' ')

    let rec pretty_atom ind a =
        match a with
        | Terminal t    -> pretty_terminal t
        | NonTerm path  -> Path.pretty path
        | Sel (a', lbo) ->
              let suff = match lbo with
                         | None    -> "."
                         | Some lb -> "." ^ lb
              in
                  (pretty_atom ind a') ^ suff

        | Sub ([], (Prod [_] as p)) -> "(" ^ (pretty_prod ind p) ^ ")"

        | Sub (decls, p) ->
              let tabs = "( " in
              let ind' = tab ind tabs in
              let d = match decls with
                      | [] -> ""
                      | _  -> (pretty_decls ind' decls) ^ ind'
              in
                  tabs ^ d ^ (pretty_prod ind' p) ^ " )"

    and pretty_terminal t =
        match t with
            Epsilon       -> "_"
          | Concat        -> "^"
          | Capitalize    -> "\\"
          | Term s	      -> "\"" ^ (String.escaped s) ^ "\""

    and pretty_seq ind (Seq (lbo, atoms, r)) =
        let lb = match lbo with
                 | None    -> ""
                 | Some lb -> lb ^ ": "
        in
          	lb ^ (mappen_strings (pretty_atom ind) ("\n" ^ ind) atoms)

    and pretty_prod ind (Prod p) = mappen_strings (pretty_seq ind) ("\n" ^ ind ^ "\b\b| ") p

    and pretty_decl ind (Bind (dm, sym, p)) =
      	let b = match dm with
           	    | Def    -> "::="
               	| Assign -> ":="
       	in
        let tabs = sym ^ " " ^ b ^ " "
        in
            tabs ^ (pretty_prod (tab ind tabs) p)

    and pretty_decls ind decls =
        (mappen_strings (pretty_decl ind) (";\n" ^ ind) decls) ^ ";\n"

  end

module Absyn2 =
  struct
    type terminal =
        Epsilon
      | Concat
      | Capitalize
      | Term of symbol
  end

module ParserAux =
  struct
    open Absyn0

    (* localize *)
    let localizes n m = Err.localize (Parsing.rhs_start n, Parsing.rhs_end m)
    let localize n = localizes n n

    (* reserved symbol *)
    let reserved_sym = "__R"

    (* shortcuts and sugars *)
    let seq a loc = (Seq (None, [a, loc]), loc)
    let sub seqs loc = Sub (Std, [], (Prod seqs, loc))
    let optsub a loc = sub [seq (Terminal Epsilon) loc; seq a loc] loc
    let add_decl d (ds, p) = (d :: ds, p)
    let reserved_nonterm = Fold (NonTerm (Path ([], reserved_sym)))

    (* a.(l1|...|ln) ---> (X ::= a; X.l1 | ... | X.ln) *)
    let multisel loc a lbs =
        let f lb = seq (Sel ((reserved_nonterm, loc), Some lb)) loc in
        let b = Bind (Def, reserved_sym, (Prod [Seq (None, [a]), loc], loc))
        in
            Fold (Sub (Std, [b, loc], (Prod (List.map f lbs), loc)))

    let posel loc atomss =
        let n = List.fold_left (fun z atoms -> max z (List.length atoms)) 1 atomss
        in
            if n = 1 then List.map List.hd atomss
            else
                let f n = function
                    | [atom] -> atom
                    | atoms  -> try List.nth atoms n
                                with _ -> Err.error loc "etherogeneous number of positional atoms in production"
                in
                let seqs = tab (fun n -> Seq (None, List.map (f n) atomss), loc) n
                in
                    [(Fold (Sub (Std, [], (Prod seqs, loc))), loc)]


    (* plus/minus expander *)

    let expand l =
        let rec init n x = if n = 0 then [] else x :: (init (n - 1) x) in
        let k =
            match List.sort (fun (n, _) (n', _) -> compare n n') l with
            | []          -> raise (Unexpected "parser.mly: expand")
            | (k, _) :: _ -> k
        in
            List.fold_left (fun z (n, x) -> z @ (init (n - k + 1) x)) [] l


    (* deep unfolder *)

    let rec deep_unfold_unfoldable = function
        | NonTerm _ as r     -> r
        | Sub (sm, decls, p) -> Sub (sm, decls, deep_unfold_prod p)

    and deep_unfold_atom (a, loc) =
        match a with
        | Fold u
        | Unfold u      -> Unfold (deep_unfold_unfoldable u), loc
        | Sel (a', lbo) -> Sel (deep_unfold_atom a', lbo), loc
        | _             -> a, loc

    and deep_unfold_seq (Seq (lbo, atoms), loc) = Seq (lbo, List.map deep_unfold_atom atoms), loc

    and deep_unfold_prod (Prod seqs, loc) = Prod (List.map deep_unfold_seq seqs), loc
  end