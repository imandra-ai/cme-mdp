:load Model/CME_Types.ml
:load Model/CME_Exchange.ml

let twelve (m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12 : int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans) = 
    true
;;

let v_t_twelve (s, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12) = 
    is_trans_valid (s, m1) && 
    is_trans_valid (s1, m2) && 
    is_trans_valid (s2, m3) && 
    is_trans_valid (s3, m4) &&
    is_trans_valid (s4, m5) && 
    is_trans_valid (s5, m6) && 
    is_trans_valid (s6, m7) && 
    is_trans_valid (s7, m8) && 
    is_trans_valid (s8, m9) && 
    is_trans_valid (s9, m10) &&
    is_trans_valid (s10, m11) && 
    is_trans_valid (s11, m12) 
;;

(** Are these transitions valid? *)
let vd_twelve (m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12 : int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans) = 
    let s   = init_state in 
    let s1  = process_int_trans (s, m1) in 
    let s2  = process_int_trans (s1, m2) in 
    let s3  = process_int_trans (s2, m3) in 
    let s4  = process_int_trans (s3, m4) in 
    let s5  = process_int_trans (s4, m5) in 
    let s6  = process_int_trans (s5, m6) in 
    let s7  = process_int_trans (s6, m7) in 
    let s8  = process_int_trans (s7, m8) in 
    let s9  = process_int_trans (s8, m9) in 
    let s10 = process_int_trans (s9, m10) in 
    let s11 = process_int_trans (s10, m11) in 
    let s12 = process_int_trans (s11, m12) in 
    v_t_twelve (s, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12)
;;

(* Disabling all the is_trans_valid *)
:disable v_t_twelve

:testgen twelve assuming vd_twelve
