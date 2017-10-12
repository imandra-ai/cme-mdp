#use "topfind";;
#require "yojson";;

:load src-model/CME_Types.ml
:load src-model/CME_Exchange.ml
:load src-model/CME_Network.ml
:load_ocaml src-model/CME.ml
:load_ocaml src-printers/CME_json.ml
:load_ocaml src-printers/CME_Exchange_json.ml
:load_ocaml src-printers/CME_Internal_json.ml
:load_ocaml src-printers/CME_Network_Printer.ml

(* A recursive run function.
   Note how this implicitly includes transition validity.

   @meta[measure : run]
     let measure_run (s, actions) = List.length actions
   @end
*)
let rec run (state, acts : network_state * net_effect list) =
    match acts with
    | [] -> { state with 
		outgoing = List.rev state.incoming @ state.outgoing;
		incoming = [] }
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

:program

(* Take the outgoing packets (testcase input) from the network state and run
   them through the CME model to produce an expected output of the testcase.
*)
let testcase_json_string_of_network_state (state : network_state) : string =
  let packet_list = List.rev state.outgoing in
  let init_state =
    { empty_feed_state with
      channels =
        { empty_feed_state.channels with
          unprocessed_packets = packet_list
        }
    } in
  let end_state = simulate init_state in
  let transitions_json = itransitions_to_json end_state.internal_changes in
  let packets_json = packets_to_json packet_list in
  let open Yojson.Basic in
  `Assoc
    [ ("input", packets_json)
    ; ("output", transitions_json)
    ]
  |> to_string
;;

:logic
