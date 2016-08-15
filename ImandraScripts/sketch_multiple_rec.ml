#use "topfind";;
#require "yojson";;

:load Model/CME_Types.ml
:load Model/CME_Exchange.ml
:load Model/CME_Network.ml

(* A recursive run function.
   Note how this implicitly includes transition validity.

   @meta[measure : run_exchange]
     let measure_run_exchange (s, actions) = List.length actions
   @end
*)
let rec run_exchange (s, acts) =
  match s, acts with
  | None  ,  _  -> s
  |     _ ,  [] -> s
  | Some s, act :: acts ->
    if is_trans_valid (s, act) then
      run_exchange ( Some (process_int_trans (s, act)) , acts)
    else None
;;
:stage run_exchange

(* A recursive run function.
   Note how this implicitly includes transition validity.

   @meta[measure : run_network]
     let measure_runnetwork (s, actions) = List.length actions
   @end
*)
let rec run_network (s, acts) =
  match s, acts with
  | None  ,  _  -> s
  |     _ ,  [] -> s
  | Some s, act :: acts ->
    if is_neteffect_valid (s, act) then
      run_network ( Some (process_net_effect (s, act)) , acts)
    else None
;;
:stage run_network

type exchange_space = {
    m1  : int_state_trans;
    m2  : int_state_trans;
    m3  : int_state_trans;
    m4  : int_state_trans;
    m5  : int_state_trans;
    m6  : int_state_trans;
    m7  : int_state_trans;
    m8  : int_state_trans;
    m9  : int_state_trans;
    m10 : int_state_trans;
    m11 : int_state_trans;
    m12 : int_state_trans;
};;
let exchange_space_to_list m = [m.m1;m.m2;m.m3;m.m4;m.m5;m.m6;m.m7;m.m8;m.m9;m.m10;m.m11;m.m12];;

type network_space = {
    n1  : net_effect;
    n2  : net_effect;
    n3  : net_effect;
    n4  : net_effect;
    n5  : net_effect;
    n6  : net_effect;
    n7  : net_effect;
    n8  : net_effect;
    n9  : net_effect;
    n10 : net_effect;
    n11 : net_effect;
    n12 : net_effect;
};;

let network_space_to_list n = [n.n1;n.n2;n.n3;n.n4;n.n5;n.n6;n.n7;n.n8;n.n9;n.n10;n.n11;n.n12];;


let no_consec_resets (m, n) =
     (m.m1  = ST_BookReset ==> not(m.m2  = ST_BookReset))
  && (m.m2  = ST_BookReset ==> not(m.m3  = ST_BookReset))
  && (m.m3  = ST_BookReset ==> not(m.m4  = ST_BookReset))
  && (m.m4  = ST_BookReset ==> not(m.m5  = ST_BookReset))
  && (m.m5  = ST_BookReset ==> not(m.m6  = ST_BookReset))
  && (m.m6  = ST_BookReset ==> not(m.m7  = ST_BookReset))
  && (m.m7  = ST_BookReset ==> not(m.m8  = ST_BookReset))
  && (m.m8  = ST_BookReset ==> not(m.m9  = ST_BookReset))
  && (m.m9  = ST_BookReset ==> not(m.m10 = ST_BookReset))
  && (m.m10 = ST_BookReset ==> not(m.m11 = ST_BookReset))
  && (m.m11 = ST_BookReset ==> not(m.m12 = ST_BookReset))
;;

let run_testgen (m, n) = 
    let exchange_state =  run_exchange(Some init_ex_state, exchange_space_to_list m) in
    match exchange_space with None -> None | Some exchange_state ->
    let network_state = { empty_network_state with incoming = exchange_space.pac_queue } in
    run_network (Some network_state, network_space_to_list n) 
;;

:adts on
:testgen run_testgen assuming no_consec_resets
