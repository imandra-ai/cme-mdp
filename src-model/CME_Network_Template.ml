#use "topfind";;
#require "yojson";;

:load src-model/CME_Types.ml
:load src-model/CME_Exchange.ml
:load src-model/CME_Network.ml
:load_ocaml src-printers/CME_json.ml
:load_ocaml src-printers/CME_Exchange_json.ml
:load_ocaml src-printers/CME_Network_Printer.ml

(* A recursive run function.
   Note how this implicitly includes transition validity.

   @meta[measure : run]
     let measure_run (s, actions) = List.length actions
   @end
*)
let rec run (state, acts : network_state * net_effect list) =
    match acts with
    | [] -> state
    | act::acts ->
        let es = process_net_effect  (state, act) in
        run ( es, acts )
;;

(* We set up run for staged symbolic execution *)
:stage run

(* @meta[measure : valid]
    let measure_valid (s, actions) = List.length actions
    @end
*)

let rec valid (state, acts : network_state * net_effect list) =
    match acts with
    | [] -> true
    | act::acts ->
        is_neteffect_valid (state, act) && (
        let es = process_net_effect (state, act) in
        valid ( es, acts) )
;;
