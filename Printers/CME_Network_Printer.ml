(* @meta[imandra_ignore] on @end *)
open CME_json;;
open CME_Network;;
(* @meta[imandra_ignore] off @end *)

let ocaml_string_of_network_state state =
  Printf.sprintf "{ %s }"
    (String.concat "; "
       [ Printf.sprintf "incoming=%s" ( state.incoming |> packets_to_ocaml )
       ; Printf.sprintf "outgoing=%s" ( state.outgoing |> packets_to_ocaml )
       ; Printf.sprintf "cache=%s"    ( state.cache    |> packets_to_ocaml )
       ])

let outgoing_packets_ocaml_string_of_network_state state =
  state.outgoing |> packets_to_ocaml

let json_string_of_network_state state =
  `Assoc
    [ ( "PacketQueue"    , state.outgoing |> packets_to_json        );
    ]
  |> Yojson.Basic.to_string
;;
