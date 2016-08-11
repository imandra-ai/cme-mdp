(* CME Testgen with staged symbolic execution in ImandraML *)
(* Requires Imandra v0.8a86 or later                       *)

:load Model/CME_Types.ml
:load Model/CME_Exchange.ml
:load_ocaml Printers/CME_Exchange_json.ml

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

(* Compute from init_ex_state through 8 transitions *)

let run_8 (m1, m2, m3, m4, m5, m6, m7, m8) =
  run (init_ex_state, [m1;m2;m3;m4;m5;m6;m7;m8])
;;

let run_12 (m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12) =
  run (init_ex_state, [m1;m2;m3;m4;m5;m6;m7;m8;m9;m10;m11;m12])
;;


(* Let's not allow consecutive resets *)

let no_consec_resets_8 (m1, m2, m3, m4, m5, m6, m7, m8) =
  (m1 = ST_BookReset ==> not(m2 = ST_BookReset))
  && (m2 = ST_BookReset ==> not(m3 = ST_BookReset))
  && (m3 = ST_BookReset ==> not(m4 = ST_BookReset))
  && (m4 = ST_BookReset ==> not(m5 = ST_BookReset))
  && (m5 = ST_BookReset ==> not(m6 = ST_BookReset))
  && (m6 = ST_BookReset ==> not(m7 = ST_BookReset))
  && (m7 = ST_BookReset ==> not(m8 = ST_BookReset))
;;

let no_consec_resets_12 (m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12) =
  (m1 = ST_BookReset ==> not(m2 = ST_BookReset))
  && (m2 = ST_BookReset ==> not(m3 = ST_BookReset))
  && (m3 = ST_BookReset ==> not(m4 = ST_BookReset))
  && (m4 = ST_BookReset ==> not(m5 = ST_BookReset))
  && (m5 = ST_BookReset ==> not(m6 = ST_BookReset))
  && (m6 = ST_BookReset ==> not(m7 = ST_BookReset))
  && (m7 = ST_BookReset ==> not(m8 = ST_BookReset))
  && (m8 = ST_BookReset ==> not(m9 = ST_BookReset))
  && (m9 = ST_BookReset ==> not(m10 = ST_BookReset))
  && (m10 = ST_BookReset ==> not(m11 = ST_BookReset))
  && (m11 = ST_BookReset ==> not(m12 = ST_BookReset))
;;

(* Some state-space exporation strategy *)

:max_region_depth 8
:max_region_time 5
:adts on

(* Finally, generate some tests, beginning with 8 transitions *)

:testgen run_8 assuming no_consec_resets_8

(* Observe: We can scale from 8 to 12 relatively linearly     *)

:testgen run_12 assuming no_consec_resets_12

