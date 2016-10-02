#use "topfind";;
#require "yojson";;

:load Model/CME_Types.ml
:load Model/CME_Exchange.ml
:load_ocaml Printers/CME_json.ml
:load_ocaml Printers/CME_Exchange_json.ml

:shadow on

let () =
    let filename = "hanging.json" in
    let s = Yojson.Basic.from_file filename |> exchange_state_of_json in
    s |> itransitions_to_json |> print_string
;;


:shadow off
