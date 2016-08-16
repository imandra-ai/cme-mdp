(**

  Script for generating test cases for the CME model.

  testgen.ml

*)

#use "topfind";;
#require "yojson";;

:load Model/CME_Types.ml
:load Model/CME_Exchange.ml
:load Model/CME_Network.ml
:load_ocaml Printers/CME_json.ml

type state = {
    exchange_state : exchange_state ;
     network_state :  network_state 
};;

type action =
    | ExchangeAction of int_state_trans
    | CopyPackets
    | NetworkAction  of net_effect
;;


(* A recursive run function.
   Note how this implicitly includes transition validity.

   @meta[measure : run]
     let measure_run (s, actions) = List.length actions
   @end
*)
let rec run (state, acts) =
    match state, acts with
    |    _ , [] -> state
    | None ,  _ -> state
    | Some s, ExchangeAction act :: acts -> 
        if is_trans_valid (s.exchange_state, act) then begin
            let es = process_int_trans (s.exchange_state, act) in 
            run (Some {s with exchange_state = es}, acts)  end 
        else None
    | Some s, NetworkAction  act :: acts -> 
        if is_neteffect_valid (s.network_state, act) then begin
            let ns = process_net_effect (s.network_state, act) in 
            run (Some { s with network_state = ns } , acts ) end 
        else None
    | Some s, CopyPackets :: acts ->
        let s = Some { s with network_state = 
            { s.network_state with 
                incoming = s.exchange_state.pac_queue
            }
        } in run ( s , acts)
;;
(* We set up run for staged symbolic execution *)
:stage run

type search_space = {
    m1  : int_state_trans;
    m2  : int_state_trans;
    m3  : int_state_trans;
    m4  : int_state_trans;
    m5  : int_state_trans;
    m6  : int_state_trans;
    m7  : int_state_trans;
    m9  : int_state_trans;
    m10 : int_state_trans;
    m11 : int_state_trans;
    m12 : int_state_trans;
    m13 : int_state_trans;
    m14 : int_state_trans;
    m15 : int_state_trans;
    m16 : int_state_trans;
    m17 : int_state_trans;
    m18 : int_state_trans;
    m19 : int_state_trans;
    m20 : int_state_trans;
    m21 : int_state_trans;
    m22 : int_state_trans;
    m23 : int_state_trans;
    m24 : int_state_trans;
    n1  : net_effect;
    n2  : net_effect;
    n3  : net_effect;
    n4  : net_effect;
    n5  : net_effect;
    n6  : net_effect;
    n7  : net_effect;
    n8  : net_effect;
    n9  : net_effect;
};;


let search_space_to_list m = [
    ExchangeAction m.m1 ;
    ExchangeAction m.m2 ;
    ExchangeAction m.m3 ;
    ExchangeAction m.m4 ;
    ExchangeAction m.m5 ;
    ExchangeAction m.m6 ;
    ExchangeAction m.m7 ;
    ExchangeAction m.m9 ;
    ExchangeAction m.m10;
    ExchangeAction m.m11;
    ExchangeAction m.m12;
    ExchangeAction m.m13;
    ExchangeAction m.m14;
    ExchangeAction m.m15;
    ExchangeAction m.m16;
    ExchangeAction m.m17;
    ExchangeAction m.m18;
    ExchangeAction m.m19;
    ExchangeAction m.m20;
    ExchangeAction m.m21;
    ExchangeAction m.m22;
    ExchangeAction m.m23;
    ExchangeAction m.m24;
    CopyPackets;
    NetworkAction  m.n1;
    NetworkAction  m.n2;
    NetworkAction  m.n3;
    NetworkAction  m.n4;
    NetworkAction  m.n5;
    NetworkAction  m.n6;
    NetworkAction  m.n7;
    NetworkAction  m.n8;
    NetworkAction  m.n9;
];;


let run_testgen m = 
    let empty_state = Some {
        exchange_state = init_ex_state;
        network_state = empty_network_state  
    } in
    run ( empty_state, search_space_to_list m ) 
;;


let no_consec_resets_8 m =
     (m.m1   = ST_BookReset ==> not(m.m2   = ST_BookReset))
  && (m.m2   = ST_BookReset ==> not(m.m3   = ST_BookReset))
  && (m.m3   = ST_BookReset ==> not(m.m4   = ST_BookReset))
  && (m.m4   = ST_BookReset ==> not(m.m5   = ST_BookReset))
  && (m.m5   = ST_BookReset ==> not(m.m6   = ST_BookReset))
  && (m.m6   = ST_BookReset ==> not(m.m7   = ST_BookReset))
  && (m.m7   = ST_BookReset ==> not(m.m9   = ST_BookReset))
  && (m.m9   = ST_BookReset ==> not(m.m10  = ST_BookReset))
  && (m.m10  = ST_BookReset ==> not(m.m11  = ST_BookReset))
  && (m.m11  = ST_BookReset ==> not(m.m12  = ST_BookReset))
  && (m.m12  = ST_BookReset ==> not(m.m13  = ST_BookReset))
  && (m.m13  = ST_BookReset ==> not(m.m14  = ST_BookReset))
  && (m.m14  = ST_BookReset ==> not(m.m15  = ST_BookReset))
  && (m.m15  = ST_BookReset ==> not(m.m16  = ST_BookReset))
  && (m.m16  = ST_BookReset ==> not(m.m17  = ST_BookReset))
  && (m.m17  = ST_BookReset ==> not(m.m18  = ST_BookReset))
  && (m.m18  = ST_BookReset ==> not(m.m19  = ST_BookReset))
  && (m.m19  = ST_BookReset ==> not(m.m20  = ST_BookReset))
  && (m.m20  = ST_BookReset ==> not(m.m21  = ST_BookReset))
  && (m.m21  = ST_BookReset ==> not(m.m22  = ST_BookReset))
  && (m.m22  = ST_BookReset ==> not(m.m23  = ST_BookReset))
  && (m.m23  = ST_BookReset ==> not(m.m24  = ST_BookReset))
;;

:shadow off
let n = ref 0;;
let write_jsons m =
    let empty_state = Some {
        exchange_state = init_ex_state;
        network_state = empty_network_state  
    } in
    let final_state = run ( empty_state, search_space_to_list m ) in
    match final_state with 
    | None -> " **** Ignoring empty test case ***** " |> print_string
    | Some final_state ->
    let packets = final_state.network_state.outgoing @ final_state.network_state.incoming in
    let () = n := !n + 1 in
    packets |> packets_to_json
            (* |> Yojson.Basic.pretty_to_string |> print_string *)
            |> Yojson.Basic.to_file (Printf.sprintf "exchange_cases/test_%d.json" !n) 
;;
:shadow on
:adts on

:testgen run_testgen assuming no_consec_resets_8  with_code write_jsons 







