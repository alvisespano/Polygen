(*
 * Polygen
 * err.ml: error handlers
 *
 * Alvise Spano' (2002-03)
 *)

open Printf

(* error locations *)

(*type loc = string * int * int * int * int*)
type loc = { filename : string; line_start : int; col_start : int; line_end : int; col_end : int }

type jump = { file: string; lines: int list ref }

let jumps : jump Stack.t = Stack.create ()

let jump file = Stack.push {file = file; lines = ref [0]} jumps

let jump_back () = ignore (Stack.pop jumps)

let next_line lexbuf =
    let lines = (Stack.top jumps).lines
    in
        lines := Lexing.lexeme_end lexbuf :: !lines

let localize (s, e) =
    let j = Stack.top jumps in
    let clip x =
        let lines' = List.filter (fun y -> y <= x) !(j.lines) in
        let base = List.hd lines'
        in
            (List.length lines', x - base)
    in
    let (ls, cs) = clip s in
    let (le, ce) = clip e
    in
        { filename = j.file; line_start = ls; col_start = cs; line_end = le; col_end = ce }


(* errors *)

let max_warning_level = 3
let warning_level = ref 1

let uwarning lv s = if lv <= !warning_level then eprintf "warning: %s\n" s else ()

let uerror s = eprintf "error: %s\n" s; exit 1

let localized_prompt s { filename = file; line_start = ls; col_start = cs; line_end = le; col_end = ce } =
    let locs =
        match (ls = le, cs = ce) with
        | true, true    -> sprintf "at line %d, col %d" ls cs
        | true, false   -> sprintf "at line %d, col %d-%d" ls cs ce
        | _             -> sprintf "from line %d, col %d to line %d, col %d" ls cs le ce
    in
        sprintf "%s: %s %s" file s locs

let localized_msg { filename = file; line_start = ls; col_start = cs; line_end = le; col_end = ce } =
    let locs =
        match (ls = le, cs = ce) with
        | true, true    -> sprintf "at line %d, col %d" ls cs
        | true, false   -> sprintf "at line %d, col %d-%d" ls cs ce
        | _             -> sprintf "from line %d, col %d to line %d, col %d" ls cs le ce
    in
        sprintf "in file '%s' %s" file locs

let error loc s = uerror (localized_prompt s loc)

let warning lv loc s = uwarning lv (localized_prompt s loc)

let info loc s = printf "%s: %s\n" loc.filename s 

(*let merror loc s ss =
	error loc s;
	iter (fun s -> eprintf "       %s\n" s) ss*)

let mwarning lv loc s ss =
	warning lv loc s;
	List.iter (fun s -> eprintf "         %s\n" s) ss

let minfo loc s ss =
    let tab = String.make (String.length loc.filename + 2) ' ' in
    info loc s;
    List.iter (fun s -> printf "%s%s\n" tab s) ss

let syntax_error file lexbuf s =
    error (localize (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)) s

let io_error s = uerror s

let arg_error s = uerror s
