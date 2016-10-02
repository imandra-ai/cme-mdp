#use "topfind";;
#require "yojson";;

:load Model/CME_Types.ml
:load Model/CME_Exchange.ml
:load_ocaml Printers/CME_Exchange_json.ml

:adts
:p (in-theory (enable IML-ADT-EXECUTABLE-COUNTERPARTS-THEORY))

(* A recursive run function.
   Note how this implicitly includes transition validity.

   @meta[measure : run]
     let measure_run (s, actions) = List.length actions
   @end
*)
let rec run (s, acts) =
  match acts with
    [] -> s
  | act :: acts ->
    if is_trans_valid (s, act) then
      run (process_int_trans (s, act), acts)
    else s
;;
(* We set up run for staged symbolic execution *)
:stage run

type t_m12 = {
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

let m12_to_list m = [m.m1;m.m2;m.m3;m.m4;m.m5;m.m6;m.m7;m.m8;m.m9;m.m10;m.m11;m.m12];;

let run_12 m = run (init_ex_state, m12_to_list m) ;;


let no_consec_resets_12 m =
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

:shadow off
let n = ref 0;;
let write_jsons m =
    let transitions = m12_to_list m in
    let transitions = transitions |> List.map state_transition_to_json in
    let transitions = `Assoc [ ( "ExchangeTransitions" , `List transitions ) ] in
    let () = n := !n + 1 in
    transitions |> Yojson.Basic.to_file (Printf.sprintf "exchange_cases/test_%d.json" !n)
;;
:shadow on

(* :max_region_depth 8 *)
(* :max_region_time 20 *)
:adts on
:testgen run_12 assuming no_consec_resets_12 with_code write_jsons
 


