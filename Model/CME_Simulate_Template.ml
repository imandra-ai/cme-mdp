#use "topfind";;
#require "yojson";;

(* @meta[imandra_ignore] on @end *)
open CME_Types;;
open CME;;
open CME_Internal_json;;
(* @meta[imandra_ignore] off @end *)

:load_ocaml Model/CME_Types.ml
:load_ocaml Model/CME.ml
:load_ocaml Printers/CME_json.ml
:load_ocaml Printers/CME_Internal_json.ml

:shadow off

let run (packet_list : packet list) : string =
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

:shadow on
