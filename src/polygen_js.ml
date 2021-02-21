open Js_of_ocaml
open Js


class type config = object
  method start : js_string Js.t optdef readonly_prop
end

let _ =
  Js.export "Polygen"
    (object%js
       method generate grammar (config : config Js.t optdef) =
        let config =
          match (Optdef.to_option config) with
          | Some config -> config
          | None -> begin
              object%js
                val start = Optdef.return (Js.string "S")
              end
          end in

        let grammar = Js.to_string grammar in
        let start_js = Optdef.get (config##.start) (fun () -> Js.string "S") in
        let start = Js.to_string start_js in

        let decls = Polygen_lib.load_decls grammar in
        match decls with
        | Ok decls -> begin
          let output = Polygen_lib.generate ~start decls in
          Js.string output
        end
        | Error e -> begin
          Js.string e
        end
     end)
