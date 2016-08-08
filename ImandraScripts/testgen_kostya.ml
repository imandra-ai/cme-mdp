#use "topfind";;
#require "yojson";;

:load Model/CME_Types.ml
:load Model/CME_Exchange.ml
:load Model/CME.ml

:load_ocaml Printers/CME_json.ml

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

let m_reg = ref [];;
let str_reg = ref [];;
let n = ref 0;;

let pipe_to_model m =
    let exchange_state = simulate_exchange ( init_ex_state , [ m.m1; m.m2; m.m3; m.m4 ] ) in
    (* This flushes the unsent messages *)
    let exchange_state = 
        if( is_trans_valid (exchange_state, ST_DataSendInc ) ) 
        then simulate_exchange ( exchange_state , [ ST_DataSendInc ] )
        else exchange_state in
    let exchange_state = 
        if( is_trans_valid (exchange_state, ST_DataSendSnap ) ) 
        then simulate_exchange ( exchange_state , [ ST_DataSendSnap ] )
        else exchange_state in
    let s = { 
        feed_sec_id   = get_security_id (exchange_state, SecA);
        feed_sec_type = SecA;
        books = { 
            book_depth = 5;
            multi    = { buys = []; sells = [] };
            implied  = { buys = []; sells = [] };
            combined = { buys = []; sells = [] };
            b_status = Empty ;
        };
        (* Communication channels *)
        channels = {
            unprocessed_packets = exchange_state.pac_queue;

            processed_messages = [];
            processed_ref_a    = [];
            processed_ref_b    = [];
            processed_snap_a   = [];
            processed_snap_b   = [];

            cycle_hist_a = clean_cycle_hist;
            cycle_hist_b = clean_cycle_hist;
            last_seq_processed = -1;    
            cache = [];       
            last_snapshot = None;
        };
        feed_status = Normal;
        internal_changes = [];
        cur_time = 0;
    } in
    (* let () = str_reg := s :: !str_reg in *)
    let s = simulate s in
    let out_json : Yojson.Basic.json = `Assoc [
        ( "ref_a", s.channels.processed_ref_a  |> packets_to_json );
        ( "ref_b", s.channels.processed_ref_b  |> packets_to_json );
        ("snap_a", s.channels.processed_snap_a |> packets_to_json );
        ("snap_b", s.channels.processed_snap_b |> packets_to_json );
    ] in
    (* let () = end_reg := s :: !end_reg in *)
    (* out_json |> Yojson.Basic.pretty_to_string |> print_string *)
    let () = n := !n + 1 in
    out_json |> Yojson.Basic.to_file (Printf.sprintf "testgen/test_%d.json" !n)
;;

:shadow on

(* Let's set max_region_time! *)
(* :max_region_time 5 *)

(*:max_region_time 5 *)
(*:max_regions 50  *)     
(* Limiting # regions to 50 so you can quickly see some results *)

:testgen four assuming valid_4_limit_resets with_code pipe_to_model
 

