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

let seed : int option ref = ref None

let get_decls0
    ?(msg=(fun _ -> ()))
    ?(lbs=LabelSet.empty)
    ?(start="S")
    source =
  let decls =
    msg ("loading source file \"" ^ source ^ "\"...");
    load_decls source in
  let _ = msg "checking grammar..."; Check.check lbs decls start in
  let _ = flush stderr; flush stdout in
  decls

let get_decls1
    ?(msg=(fun _ -> ()))
    source =
  let decls0 = get_decls0 source in
  msg "preprocessing grammar...";
  Pre.pre decls0

let pRNG_init () =
  match !seed with
  | None   -> Random.self_init (); seed := Some (Random.bits ())
  | Some s -> Random.init s

let generate
    ?(msg=(fun _ -> ()))
    ?(eof="\n")
    ?(lbs=LabelSet.empty)
    ?(start="S")
    ?(times=1)
    ?(dest=stdout)
    source =
  (* load complied or source grammar *)
  let decls =
    try
      msg "loading compiled grammar...";
      load_obj source
    with Failure s -> msg s; get_decls1 source in

  (* generate *)
  msg ("PRNG seed: " ^ (string_of_int (surely_some !seed)));
  msg ("EOF string: \"" ^ (String.escaped eof) ^ "\"");
  msg ("initial label environment: " ^ LabelSet.pretty lbs) ;
  msg ("generator output:");
  for i = 1 to times do
    fprintf dest "%s%s" (Gen.gen lbs decls start) eof
  done;

  (* store compiled grammar *)
  try
    msg "storing compiled grammar...";
    store_obj source decls
  with Failure s -> msg s