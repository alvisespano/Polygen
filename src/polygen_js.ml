open Js_of_ocaml
open Js


class type config = object
  method start : js_string t optdef readonly_prop
  method lbs : js_string t js_array t optdef readonly_prop
end

let _ =
  export "Polygen"
    (object%js
       method generate grammar (config : config t optdef) =
        let config =
          match (Optdef.to_option config) with
          | Some config -> config
          | None -> begin
              object%js
                val start = Optdef.return (string "S")
                val lbs = Optdef.return (array [||])
              end
          end in

        let grammar = to_string grammar in
        let start = Optdef.get (config##.start) (fun () -> string "S") |> to_string in
        let lbs =
          Optdef.get (config##.lbs) (fun () -> array [||])
          |> to_array
          |> Array.to_list
          |> List.map to_string
          |> Polygen_lib.Prelude.LabelSet.of_labels in

        let decls = Polygen_lib.load_decls grammar in
        match decls with
        | Ok decls -> begin
          let output = Polygen_lib.generate ~lbs ~start decls in
          string output
        end
        | Error e -> begin
          string e
        end
      method pRngInit (seed : int) : int =
        Polygen_lib.pRNG_init ~seed:(Some seed) ()
     end)
