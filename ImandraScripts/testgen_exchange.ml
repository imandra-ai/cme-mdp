#use "topfind";;
#require "yojson";;

:load Model/CME_Types.ml
:load Model/CME_Exchange.ml
:load_ocaml Printers/CME_Exchange_json.ml

:adts
:p (in-theory (enable IML-ADT-EXECUTABLE-COUNTERPARTS-THEORY))

let int_of_book_reset x = match x with ST_BookReset -> 1 | _ -> 0 ;;

type m_four = {
    m1 : int_state_trans;
    m2 : int_state_trans;
    m3 : int_state_trans;
    m4 : int_state_trans;
};;

let four ( m : m_four ) = true ;;

let valid_trans_4_s ( s1, s2, s3, s4, m ) = 
       is_trans_valid (s1, m.m1)
    && is_trans_valid (s2, m.m2)
    && is_trans_valid (s3, m.m3)
    && is_trans_valid (s4, m.m4)
;;

(** Are these transitions valid? *)
let valid_trans_4 ( m : m_four ) = 
    let s1  = init_ex_state in 
    let s2  = process_int_trans (s1, m.m1) in 
    let s3  = process_int_trans (s2, m.m2) in 
    let s4  = process_int_trans (s3, m.m3) in 

    valid_trans_4_s ( s1, s2, s3, s4, m ) 
;;

let valid_4_limit_resets ( m : m_four ) =   
  (* No more than two resets *)
  int_of_book_reset m.m1 + int_of_book_reset m.m2 + int_of_book_reset m.m3 + int_of_book_reset m.m4 <= 2
  (* Transitions valid *)
  && valid_trans_4 m
;;


:shadow off
let n = ref 0;;

let write_jsons m =
    let transitions = [ m.m1; m.m2; m.m3; m.m4 ] in
    let transitions = transitions |> List.map state_transition_to_json in
    let transitions = `Assoc [ ( "ExchangeTransitions" , `List transitions ) ] in
    let () = n := !n + 1 in
    transitions |> Yojson.Basic.to_file (Printf.sprintf "exchange_cases/test_%d.json" !n)
;;
 
:shadow on

:testgen four assuming valid_4_limit_resets with_code write_jsons

