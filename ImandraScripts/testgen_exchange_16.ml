#use "topfind";;
#require "yojson";;

:load Model/CME_Types.ml
:load Model/CME_Exchange.ml
:load_ocaml Printers/CME_Exchange_json.ml

:adts on

:disable init_state

(* A recursive run function.
   Note how this implicitly includes transition validity.

   @meta[measure : run]
     let measure_run (s, actions) = List.length actions
   @end
*)
let rec run (s, acts) =
  match s, acts with
  | Some s, [] -> Some s
  | Some s, act :: acts ->
    if is_trans_valid (s, act) then
      run (Some (process_int_trans (s, act)), acts)
    else
      None
  | None, _ -> None
;;

(* We set up run for staged symbolic execution *)
:stage run

type t_m16 = {
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
    m13 : int_state_trans;
    m14 : int_state_trans;
    m15 : int_state_trans;
    m16 : int_state_trans;
};;

let m16_to_list m = [m.m1;m.m2;m.m3;m.m4;m.m5;m.m6;m.m7;m.m8;m.m9;m.m10;m.m11;m.m12;m.m13;m.m14;m.m15;m.m16];;

let run_16 m = run (Some init_ex_state, m16_to_list m) ;;

let no_consec_resets_16 m =
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
  && (m.m12 = ST_BookReset ==> not(m.m13 = ST_BookReset))
  && (m.m13 = ST_BookReset ==> not(m.m14 = ST_BookReset))
  && (m.m14 = ST_BookReset ==> not(m.m15 = ST_BookReset))
  && (m.m15 = ST_BookReset ==> not(m.m16 = ST_BookReset))
;;

:shadow off
let n = ref 0;;
let write_jsons m =
    let transitions = m16_to_list m in
    let transitions = transitions |> List.map state_transition_to_json in
    let transitions = `Assoc [ ( "ExchangeTransitions" , `List transitions ) ] in
    let () = n := !n + 1 in
    transitions |> Yojson.Basic.to_file (Printf.sprintf "exchange_cases/test_%d.json" !n)
;;
:shadow on

(* :max_region_depth 8 *)
(* :max_region_time 20 *)

:adts on
:max_region_time 10
:tcs_per_region 10                 (* Get 10 test-cases per region *)

(* Subtree 10 has interesting behaviour, so let us go there first. *)

:testgen run_16 assuming no_consec_resets_16 with_code write_jsons subtree 10

(* Subtree 22 is also interesting... *)

:testgen run_16 assuming no_consec_resets_16 with_code write_jsons subtree 22

