#use "topfind";;
#require "yojson";;

:load Model/CME_Types.ml
:load Model/CME_Exchange.ml
:load Model/CME_Network.ml
:load_ocaml Printers/CME_json.ml
:load_ocaml Printers/CME_Exchange_json.ml

type action =
    | BookAction     of book_transition
    | ExchangeAction of exchange_transition
;;


(* A recursive run function.
   Note how this implicitly includes transition validity.

   @meta[measure : run]
     let measure_run (s, actions) = List.length actions
   @end
*)
let rec run (state, acts) =
    match acts with
    | [] -> state
    | BookAction act :: acts ->
        let es = process_book_trans (state, act) in
        run (es, acts)
    | ExchangeAction act :: acts ->
        let es = process_exchange_trans (state, act) in
        run (es, acts)
;;

(* We set up run for staged symbolic execution *)
:stage run

(* @meta[measure : valid]
    let measure_valid (s, actions) = List.length actions
    @end
*)

let rec valid (s, acts) =
    match acts with
    | [] -> true
    | BookAction act :: acts ->
        is_book_trans_valid (s, act) && (
        let es = process_book_trans (s, act) in
        valid (es, acts) )
    | ExchangeAction act :: acts ->
        is_exchange_trans_valid (s, act) && (
        let es = process_exchange_trans (s, act) in
        valid (es, acts) )
;;
