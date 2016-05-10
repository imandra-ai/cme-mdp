
(**

  Script for generating test cases for the CME model.

  testgen.ml

*)

(** This is an indicator for Imandra that the following should not be processed. *)

(* @meta[imandra_ignore] on @end *)
open CME;;
(* @meta[imandra_ignore] off @end *)

let books_are_not_empty (b : books) =
    b.book_depth = 5 && 
    b.multi.buys = [] &&
    b.multi.sells = [] &&
    b.implied.buys = [] && 
    b.implied.sells = [] &&
    b.combined.buys = [] &&
    b.combined.sells = [] &&
    b.b_status = Publishable
;;

let rec all_msgs_correct (msgs, depth : ref_packet list * int) =
    match msgs with
    | [] -> true
    | x::xs ->
        if not (correct_level(x.rp_msg, depth)) ||
        x.rp_msg.rm_price_level < 0 ||  
        97000 < x.rp_msg.rm_entry_px || 
        x.rp_msg.rm_rep_seq_num <= 0 || 
        x.rp_header.ph_packet_seq_num <= 0 ||
        x.rp_header.ph_sending_time <= 0
        then false
        else
            all_msgs_correct (xs, depth)
;;

let books_are_not_empty_1 (b : books) =  
    b.multi = 
    {buys =
      [Level {side = BUY; qty = 100; price = 99500; num_orders = Some 100};
       Level {side = BUY; qty = 100; price = 99000; num_orders = Some 100};
       Level {side = BUY; qty = 100; price = 98500; num_orders = Some 100};
       Level {side = BUY; qty = 100; price = 98000; num_orders = Some 100};
       Level {side = BUY; qty = 100; price = 97500; num_orders = Some 100}];
     sells =
      [Level {side = SELL; qty = 100; price = 100000; num_orders = Some 100};
       Level {side = SELL; qty = 100; price = 100500; num_orders = Some 100};
       Level {side = SELL; qty = 100; price = 101000; num_orders = Some 100};
       Level {side = SELL; qty = 100; price = 101500; num_orders = Some 100};
       Level {side = SELL; qty = 100; price = 102000; num_orders = Some 100}]} &&

    b.implied =
    {buys =
      [];
     sells =
      []} && 

    b.combined = 
    {buys =
      [Level {side = BUY; qty = 100; price = 99500; num_orders = Some 100};
       Level {side = BUY; qty = 100; price = 99000; num_orders = Some 100};
       Level {side = BUY; qty = 100; price = 98500; num_orders = Some 100};
       Level {side = BUY; qty = 100; price = 98000; num_orders = Some 100};
       Level {side = BUY; qty = 100; price = 97500; num_orders = Some 100}];
     sells =
      [Level {side = SELL; qty = 100; price = 100000; num_orders = Some 100};
       Level {side = SELL; qty = 100; price = 100500; num_orders = Some 100};
       Level {side = SELL; qty = 100; price = 101000; num_orders = Some 100};
       Level {side = SELL; qty = 100; price = 101500; num_orders = Some 100};
       Level {side = SELL; qty = 100; price = 102000; num_orders = Some 100}]}
;;

(** Make sure that the channels are consistent *)
let channels_are_good ( ch, b : channels * books ) =
  all_msgs_correct (ch.ref_a.r_unproc_packets, b.book_depth) && 
  all_msgs_correct (ch.ref_b.r_unproc_packets, b.book_depth) && 
  b.book_depth > 1 && 
  ch.ref_a.r_proc_packets = [] &&
  ch.ref_b.r_proc_packets = [] &&
  ch.snap_a.s_proc_packets = [] &&
  ch.snap_b.s_proc_packets = [] &&
  ch.cache = [] &&
  ch.cycle_hist_a.reference_sec_id = ch.cycle_hist_b.reference_sec_id &&
  ch.cycle_hist_a.liq = LiqUnknown &&
  ch.cycle_hist_b.liq = LiqUnknown &&
  ch.cycle_hist_a.ref_sec_snap_received = false &&
  ch.cycle_hist_b.ref_sec_snap_received = false
;;

(** Make sure the initial state is good *)
let init_empty_state (s : feed_state) =
  s.sec_type = FUTURES &&
  s.channels.cycle_hist_a.reference_sec_id <> s.sec_id &&
  s.channels.cycle_hist_a.self_sec_id = s.sec_id &&
  s.channels.cycle_hist_b.self_sec_id = s.sec_id &&
  s.sec_id > 0 && s.sec_id < 10 &&
  s.internal_changes = [] &&
  s.cur_time = 0 &&
  s.feed_status = Normal
;;

:!disable 
  get_next_packet
  sort_book
  is_cache_valid_since_seq_num
  process_msg_recovery
  attempt_recovery
  recalc_combined
  bk_delete_from
  insert_level
  books_are_not_empty_1
  all_msgs_correct
;;

:!enable
get_next_packet
process_ch
set_channel 
clean_multi_depth_book
trim_side
add_levels 
delete_level
process_snap_ch
process_ref_ch 
set_snap_channel
set_ref_channel
msg_behind
;;

let side_cond (s: feed_state) =
  books_are_not_empty_1 (s.books)
  && s.books.book_depth = 5 
  && s.books.b_status = Publishable
  && init_empty_state (s)
  && channels_are_good (s.channels, s.books)
;;

:unroll 6

:load tsg_help.ml

:load_ocaml CME_test_helper.ml
:load_ocaml CME_printers.ml
:load_ocaml CME_to_json_simple.ml
:load_ocaml CME_test_printer_del.ml

:tcs_per_region 5


let large_call (s, msg1, msg2, msg3, msg4, msg5, msg6, msg7, msg8) =
  let s' = one_step(s) in 
  let s' = one_step(s') in 
  let s' = one_step(s') in 
  let s' = one_step(s') in 
  let s' = one_step(s') in 
  let s' = one_step(s') in 
  let s' = one_step(s') in 
  one_step(s')
;;

let large_call' (s, msg1, msg2, msg3, msg4, msg5, msg6, msg7, msg8) =
  let s' = {
    s with
      books = {
        book_depth = 5;
        multi = 
        {buys =
          [Level {side = BUY; qty = 100; price = 99500; num_orders = Some 100};
           Level {side = BUY; qty = 100; price = 99000; num_orders = Some 100};
           Level {side = BUY; qty = 100; price = 98500; num_orders = Some 100};
           Level {side = BUY; qty = 100; price = 98000; num_orders = Some 100};
           Level {side = BUY; qty = 100; price = 97500; num_orders = Some 100}];
         sells =
          [Level {side = SELL; qty = 100; price = 100000; num_orders = Some 100};
           Level {side = SELL; qty = 100; price = 100500; num_orders = Some 100};
           Level {side = SELL; qty = 100; price = 101000; num_orders = Some 100};
           Level {side = SELL; qty = 100; price = 101500; num_orders = Some 100};
           Level {side = SELL; qty = 100; price = 102000; num_orders = Some 100}]};

        implied =
        {buys =
          [];
         sells =
          []};

        combined = 
        {buys =
          [Level {side = BUY; qty = 100; price = 99500; num_orders = Some 100};
           Level {side = BUY; qty = 100; price = 99000; num_orders = Some 100};
           Level {side = BUY; qty = 100; price = 98500; num_orders = Some 100};
           Level {side = BUY; qty = 100; price = 98000; num_orders = Some 100};
           Level {side = BUY; qty = 100; price = 97500; num_orders = Some 100}];
         sells =
          [Level {side = SELL; qty = 100; price = 100000; num_orders = Some 100};
           Level {side = SELL; qty = 100; price = 100500; num_orders = Some 100};
           Level {side = SELL; qty = 100; price = 101000; num_orders = Some 100};
           Level {side = SELL; qty = 100; price = 101500; num_orders = Some 100};
           Level {side = SELL; qty = 100; price = 102000; num_orders = Some 100}]};

        b_status = Publishable;
      };

      channels = { 
        s.channels with ref_a = { s.channels.ref_a with r_unproc_packets = [msg1; msg2; msg3] };
                        ref_b = { s.channels.ref_b with r_unproc_packets = [msg4; msg5] };
                        snap_a = { s.channels.snap_a with s_unproc_packets = [msg6; msg7; msg8] };
                        snap_b = { s.channels.snap_b with s_unproc_packets = [] };
      };

  } in

  let s' = one_step(s') in 
  let s' = one_step(s') in 
  let s' = one_step(s') in 
  let s' = one_step(s') in 
  let s' = one_step(s') in 
  let s' = one_step(s') in 
  let s' = one_step(s') in 
  one_step(s')
;;

(*

:testgen large_call with_printer cme_test_printer_ext

:testgen process_msg_normal assuming side_cond with_printer cme_test_printer
:testgen process_msg_recovery 

:testgen one_step assuming side_cond with_printer cme_test_printer

:testgen one_step with_printer cme_test_printer
*)


