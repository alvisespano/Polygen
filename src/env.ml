(*
 * Polygen
 * env.ml: environment
 *
 * (c) 2002, 2003, 2004, 2015 Alvise Spano'
 *)

open Prelude
open Absyn

  	  
module M = Map.Make (struct type t = path let compare = compare end)
    
type 'a t = 'a M.t

let empty = M.empty

let bind env x v = M.add x v env

let binds env bs = List.fold_left (fun env (x, v) -> bind env x v) env bs

let lookup env path = M.find path env
	

(* old env: does not use paths as key *)
(*module Env =
  struct

    type 'a t = (symbol * 'a) list

    let empty : 'a t = []

    let bind (env : 'a t) xs =  xs @ env

    let lookup (env : 'a t) = function
        Path ([], sym) -> List.assoc sym env
      | Path (_, _)    -> raise (Unexpected "env.ml: lookup")

    (*let find (env : 'a t) f x =
        let sym = fst (List.find (fun (_, x') -> x = f x') env)
        in
            Path ([], sym)*)

  end*)

