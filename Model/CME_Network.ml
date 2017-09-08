(**

    Aesthetic Integration Ltd.
    Copyright 2016

    CME_Network.ml

*)



(* @meta[imandra_ignore] on @end *)
open CME_Types;;
(* @meta[imandra_ignore] off @end *)

type net_effect = NoEffect | PacketLoss | PacketMoveToCache | PacketMoveFromCache of int;;


(** Module representing the *)
type network_state = {
    incoming : packet list;
    outgoing : packet list;
    cache    : packet list;
};; 

let empty_network_state = {
    incoming = []; outgoing = []; cache = []
};;

(** Is the transition valid? *)
let is_neteffect_valid (n, e : network_state * net_effect) =
    let queue_not_empty = n.incoming <> [] in  
    match e with 
    | NoEffect              -> queue_not_empty
    | PacketLoss            -> queue_not_empty
    | PacketMoveToCache     -> queue_not_empty
    | PacketMoveFromCache x -> 0 <= x && x < List.length n.cache
;;

(** Get n_th element from the list *)
let rec get_nth ( packets, n, idx : packet list * int * int ) = 
    match packets with 
    | h::tl -> if idx = n then h else get_nth ( tl, n, idx+1 )
    | [] -> {
        packet_seq_num  = 0;
        packet_messages = [];
        packet_channel  = Ch_Ref_A 
    }
;;

(** Remove n_th element from the list *)
let rec remove_nth ( packets, n, idx : packet list * int * int ) = 
    match packets with 
    | h::tl -> if idx = n then tl else h :: remove_nth ( tl, n, idx+1 )
    | [] -> []
;;

(** Process network effect *)
let process_net_effect (n, e : network_state * net_effect) =
    match e with 
    | NoEffect              -> { n with incoming = List.tl n.incoming;
                                        outgoing = List.hd n.incoming :: n.outgoing }
    | PacketLoss            -> { n with incoming = List.tl n.incoming; }
    | PacketMoveToCache     -> begin
        match n.incoming with 
        | [] -> n
        | h::tl -> { n with incoming = tl; cache = h::n.cache }
        end
    | PacketMoveFromCache x -> begin
        match n.cache with 
        | [] -> n
        | h::tl -> { n with cache = remove_nth (n.cache, x, 0 ) ; outgoing = (get_nth (n.cache, x, 0))::n.outgoing }
        end
;;

(** Simulate  *)
let rec simulate_network (n, effects : network_state * net_effect list ) =
    match effects with 
    | [] -> n 
    | x::xs -> simulate_network ( process_net_effect (n, x), xs)
;;
