(**

    Aesthetic Integration Ltd.
    Copyright 2016

    CME_Network.ml

*)



type net_effect = PacketLoss | NetworkSlow | Network

(** Module representing the *)
type network_state = {

    incoming : packet list;
    outgoing : packet list;
    cache    : packet list;

    (** changes *)
    next_net_effect : net_effect option;
};; 


(** Is the transition valid? *)
let is_trans_valid (n, e : network_state, net_effect) =
    let queue_not_empty = n.incoming <> [] in  
    match e with 
    | NoEffect              -> true
    | PacketLoss            -> queue_not_empty
    | PacketMoveToCache     -> queue_not_empty
    | PacketMoveFromCache x -> 0 <= x && x < List.length n.cache
;;

(** Remove n_th element from the list *)
let rec remove_nth ( packets, n, idx : packet list * int * int ) = 

;;

(** Process network effect *)
let process_net_effect (n, e : network_state, net_effect) =
    match e with 
    | NoEffect              -> n
    | PacketLoss            -> { n with incoming = List.tl n.incoming; }
    | PacketMoveToCache     -> 
        match n.incoming with 
        | [] -> n
        | x::xs -> { n with inoming = xs; cache = }

    | PacketMoveFromCache x -> 
        match n.cache with 
        | [] -> 

;;

(** Simulate  *)
let rec simulate_network (n, effects : network_state * net_effect list ) =
    match effects with 
    | [] -> n 
    | x::xs -> simulate_network ( process_net_effect (n, x), xs)
;;