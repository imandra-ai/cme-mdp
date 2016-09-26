#use "topfind";;
#require "yojson";;

:load Model/CME_Types.ml
:load Model/CME_Exchange.ml
:load_ocaml Printers/CME_json.ml
:load_ocaml Printers/CME_Exchange_json.ml

let loaded_state =
    let filename = "hanging.json" in
    Yojson.Basic.from_file filename |> exchange_state_of_json
;;

let g x =
  if x > 22 then 9
  else 100 + x;;

let f x =
  if x > 99 then
    100
  else if x < 70 && x > 23
  then 89 + x
  else if x > 20
  then g x + 20
  else if x > -2 then
    103
  else 99;;

:web on
:decompose f
