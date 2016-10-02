#use "topfind";;
#require "yojson";;

:load Model/CME_Types.ml
:load Model/CME_Exchange.ml
:load_ocaml Printers/CME_json.ml
:load_ocaml Printers/CME_Exchange_json.ml

:shadow on

let loaded_state =
    let filename = "hanging.json" in
    Yojson.Basic.from_file filename |> exchange_state_of_json
;;

Marshal.to_string [] loaded_state |> print_string;;

:shadow off 
