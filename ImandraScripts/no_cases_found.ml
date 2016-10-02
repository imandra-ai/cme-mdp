#use "topfind";;
#require "yojson";;

:load Model/CME_Types.ml
:load Model/CME_Exchange.ml
:load_ocaml Printers/CME_Exchange_json.ml

:adts
:p (in-theory (enable IML-ADT-EXECUTABLE-COUNTERPARTS-THEORY))

let int_of_book_reset x = match x with ST_BookReset -> 1 | _ -> 0 ;;

type m_eight = {
    m1 : int_state_trans;
    m2 : int_state_trans;
    m3 : int_state_trans;
    m4 : int_state_trans;
    m5 : int_state_trans;
    m6 : int_state_trans;
    m7 : int_state_trans;
    m8 : int_state_trans;
};;

let eight ( m : m_eight ) = true ;;

let valid_trans_8_s ( s1, s2, s3, s4, s5, s6, s7, s8, m ) = 
       is_trans_valid (s1, m.m1)
    && is_trans_valid (s2, m.m2)
    && is_trans_valid (s3, m.m3)
    && is_trans_valid (s4, m.m4)
    && is_trans_valid (s5, m.m5)
    && is_trans_valid (s6, m.m6)
    && is_trans_valid (s7, m.m7)
    && is_trans_valid (s8, m.m8)
;;

(** Are these transitions valid? *)
let valid_trans_8 ( m : m_eight ) = 
    let s1  = init_ex_state in 
    let s2  = process_int_trans (s1, m.m1) in 
    let s3  = process_int_trans (s2, m.m2) in 
    let s4  = process_int_trans (s3, m.m3) in 
    let s5  = process_int_trans (s4, m.m4) in 
    let s6  = process_int_trans (s5, m.m5) in 
    let s7  = process_int_trans (s6, m.m6) in 
    let s8  = process_int_trans (s7, m.m7) in 

    valid_trans_8_s ( s1, s2, s3, s4, s5, s6, s7, s8, m ) 
;;

let valid_8_limit_resets ( m : m_eight ) =   
  (* No more than two resets *)
  int_of_book_reset m.m1 + int_of_book_reset m.m2 + int_of_book_reset m.m3 + int_of_book_reset m.m4 +
  int_of_book_reset m.m5 + int_of_book_reset m.m6 + int_of_book_reset m.m7 + int_of_book_reset m.m8 <= 1
  (* Transitions valid *)
  && valid_trans_8 m
;;


:shadow off
let n = ref 0;;

let write_jsons m =
    let transitions = [ m.m1; m.m2; m.m3; m.m4; m.m5; m.m6; m.m7; m.m8 ] in
    let transitions = transitions |> List.map state_transition_to_json in
    let transitions = `Assoc [ ( "ExchangeTransitions" , `List transitions ) ] in
    let () = n := !n + 1 in
    transitions |> Yojson.Basic.to_file (Printf.sprintf "exchange_cases/test_%d.json" !n)
;;
 
:shadow on

:max_region_depth 8
:testgen eight assuming valid_8_limit_resets with_code write_jsons
 


