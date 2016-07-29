(* Stuck.ml updated with max_region_time examples *)

:load Model/CME_Types.ml
:load Model/CME_Exchange.ml

:adts
:p (in-theory (enable IML-ADT-EXECUTABLE-COUNTERPARTS-THEORY))


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
    let s   = init_ex_state in 
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

let int_of_book_reset (x : int_state_trans) =
  match x with
    ST_BookReset -> 1
  | _ -> 0
;;

let vd_twelve_limit_resets (m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12 : int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans * int_state_trans) =   
  (* No more than 3 resets *)
  int_of_book_reset m1 + int_of_book_reset m2 + int_of_book_reset m3
  + int_of_book_reset m4 + int_of_book_reset m5 + int_of_book_reset m6 <= 2
  && int_of_book_reset m7 + int_of_book_reset m8 + int_of_book_reset m9
     + int_of_book_reset m10 + int_of_book_reset m11 + int_of_book_reset m12 <= 1
  (* Transitions *)
  && vd_twelve (m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12)
;;




  
(* ****************************** *)  
(*  A version with 4 transitions  *)
(* ****************************** *)  

  
let four (m1, m2, m3, m4 : int_state_trans * int_state_trans * int_state_trans * int_state_trans) = 
    true
;;

let valid_trans_4_s (s1, s2, s3, s4, m1, m2, m3, m4) = 
    is_trans_valid (s1, m1)
    && is_trans_valid (s2, m2)
    && is_trans_valid (s3, m3)
    && is_trans_valid (s4, m4)
;;

(** Are these transitions valid? *)
let valid_trans_4 (m1, m2, m3, m4 : int_state_trans * int_state_trans * int_state_trans * int_state_trans) = 
    let s1  = init_ex_state in 
    let s2  = process_int_trans (s1, m1) in 
    let s3  = process_int_trans (s2, m2) in 
    let s4  = process_int_trans (s3, m3) in 
    valid_trans_4_s (s1, s2, s3, s4, m1, m2, m3, m4)
;;

let valid_4_limit_resets (m1, m2, m3, m4 : int_state_trans * int_state_trans * int_state_trans * int_state_trans) =   
  (* No more than two resets *)
  int_of_book_reset m1 + int_of_book_reset m2 + int_of_book_reset m3 + int_of_book_reset m4 <= 2
  (* Transitions valid *)
  && valid_trans_4 (m1,m2,m3,m4)
;;


  
(* ---- BREAK ---- *)  
:break
(* ---- BREAK ---- *)  

   

(* Let's set max_region_time! *)
   
:max_region_time 5
:testgen four assuming valid_4_limit_resets
   
:max_region_time 1
:testgen twelve assuming vd_twelve	 

(* Examples of time-limits that are too low *)
	 
:max_region_time 1
:testgen twelve assuming vd_twelve_limit_resets

:max_region_time 2
:testgen twelve assuming vd_twelve_limit_resets

(* On my machine, 7 seconds is enough. Your mileage may vary. *)
	 
:max_region_time 7
:testgen twelve assuming vd_twelve_limit_resets
	 
:max_region_time 10
:testgen twelve assuming vd_twelve_limit_resets

(* Examples with subtrees *)
	 
:max_region_time 1
:testgen twelve assuming vd_twelve subtree 14

:max_region_time 1
:testgen twelve assuming vd_twelve subtree 4

:max_region_time 1
:testgen twelve assuming vd_twelve subtree 2
	 
:testgen twelve assuming vd_twelve subtree 1

