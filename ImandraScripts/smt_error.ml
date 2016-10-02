:load Model/CME_Types.ml
:load Model/CME_Exchange.ml

:adts
:p (in-theory (enable IML-ADT-EXECUTABLE-COUNTERPARTS-THEORY))

(*
let twelve (m1, m2, m3, m4, m5, m6 : int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans) = 
    true
;;
let v_t_twelve (s, s1, s2, s3, s4, s5, s6, m1, m2, m3, m4, m5, m6) = 
    is_trans_valid (s, m1) && 
    is_trans_valid (s1, m2) && 
    is_trans_valid (s2, m3) && 
    is_trans_valid (s3, m4) &&
    is_trans_valid (s4, m5) && 
    is_trans_valid (s5, m6) 
;;
(** Are these transitions valid? *)
let vd_twelve (m1, m2, m3, m4, m5, m6 : int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans) = 
    let s   = init_state in 
    let s1  = process_int_trans (s, m1) in 
    let s2  = process_int_trans (s1, m2) in 
    let s3  = process_int_trans (s2, m3) in 
    let s4  = process_int_trans (s3, m4) in 
    let s5  = process_int_trans (s4, m5) in 
    let s6  = process_int_trans (s5, m6) in 
    v_t_twelve (s, s1, s2, s3, s4, s5, s6, m1, m2, m3, m4, m5, m6)
;;
*)
let twelve (m1, m2, m3 : int_state_trans * int_state_trans * int_state_trans ) = 
    true
;;
let v_t_twelve (s, s1, s2, m1, m2, m3) = 
    is_trans_valid (s, m1) && 
    is_trans_valid (s1, m2) && 
    is_trans_valid (s2, m3)
;;
(** Are these transitions valid? *)
let vd_twelve (m1, m2, m3 : int_state_trans * int_state_trans * int_state_trans ) = 
    let s   = init_state in 
    let s1  = process_int_trans (s, m1) in 
    let s2  = process_int_trans (s1, m2) in 
    let s3  = process_int_trans (s2, m3) in
    v_t_twelve (s, s1, s2, m1, m2, m3)
;;
(* Disabling all the is_trans_valid *)
:disable v_t_twelve
:testgen twelve assuming vd_twelve
