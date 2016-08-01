(* Stuck.ml updated with max_region_time and with_code examples                *)
(* Kostya, please see the example immediately underneath the :break command    *)

:load Model/CME_Types.ml
:load Model/CME_Exchange.ml

:adts
:p (in-theory (enable IML-ADT-EXECUTABLE-COUNTERPARTS-THEORY))

(* ****************************** *)
(*  A version with 4 transitions  *)
(* ****************************** *)


let four (m1, m2, m3, m4 : int_state_trans * int_state_trans * int_state_trans * int_state_trans) =
    true
;;

(** Are these transitions valid? *)
let valid_trans_4 (m1, m2, m3, m4 : int_state_trans * int_state_trans * int_state_trans * int_state_trans) =
    let s1  = init_ex_state in
    let s2  = process_int_trans (s1, m1) in
    let s3  = process_int_trans (s2, m2) in
    let s4  = process_int_trans (s3, m3) in
    valid_trans_4_s (s1, s2, s3, s4, m1, m2, m3, m4)
;;

let int_of_book_reset x =
  match x with
    ST_BookReset -> 1
  | _ -> 0
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



(* Let's set max_region_time and define a custom with_code test case processor! *)
(* We're going to make the with_code processor record our test cases for us in a global *)

:max_region_time 5
:max_regions 50       (* Limiting # regions to 50 so you can quickly see some results *)

:shadow off

let my_regions = ref [];;

let record_region x =
  my_regions := x :: !my_regions
;;

:shadow on

:testgen four assuming valid_4_limit_resets with_code record_region

(* Now, inspect my_regions *)

my_regions;;

(* We can now write whatever code we want to iterate through
   !my_regions and put the combinations in different channels. *)
