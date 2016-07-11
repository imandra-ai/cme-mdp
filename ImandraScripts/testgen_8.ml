:load Model/CME_Types.ml
:load Model/CME.ml

:load_ocaml Model/CME_test_helper.ml
:load_ocaml Printers/CME_printers.ml
:load_ocaml Printers/kojson.ml
:load_ocaml Printers/CME_json.ml
:load_ocaml Printers/CME_test_printer_del.ml

:adts
:p (in-theory (enable IML-ADT-EXECUTABLE-COUNTERPARTS-THEORY))

:!disable
 add_to_cache
 apply_update_packets
 delete_level
 insert_level
 insert_order
 order_higher_ranked
 update_cycle_hist
 is_cache_sorted
 recalc_combined  
;;

 let good_levels ( msg1, msg2, msg3, msg4, msg5, msg6, msg7, msg8) = 
    msg1.rp_msg.rm_price_level > 0  && msg1.rp_msg.rm_price_level <= 5 &&
    msg2.rp_msg.rm_price_level > 0  && msg2.rp_msg.rm_price_level <= 5 && 
    msg3.rp_msg.rm_price_level > 0  && msg3.rp_msg.rm_price_level <= 5 && 
    msg4.rp_msg.rm_price_level > 0  && msg4.rp_msg.rm_price_level <= 5 && 
    msg5.rp_msg.rm_price_level > 0  && msg5.rp_msg.rm_price_level <= 5 ;;


let large_call (msg1, msg2, msg3, msg4, msg5, msg6, msg7, msg8) =
    let s = { 
        books = { 
            book_depth = 5; multi = {buys = []; sells = []};
            implied = {buys = []; sells = []};
            combined = {buys = []; sells = []};
            b_status = Publishable; 
        };
        channels = {  
            ref_a  = { r_unproc_packets = [msg1; msg2; msg3]; r_proc_packets=[] };
            ref_b  = { r_unproc_packets = [msg4; msg5];       r_proc_packets=[] };
            snap_a = { s_unproc_packets = [msg6; msg7; msg8]; s_proc_packets=[] };
            snap_b = { s_unproc_packets = []; 		      s_proc_packets=[] };
            last_seq_processed = 0;
            last_snapshot = None;
            cache = [];
	    cycle_hist_a = { reference_sec_id = 10;
                             self_sec_id = 123;
                             ref_sec_snap_received = false;
                             liq = Liquid;
                            };
            cycle_hist_b = { reference_sec_id = 10;
                             self_sec_id = 123;
                             ref_sec_snap_received = false;
                             liq = Liquid;
                           }; 
	    };
       feed_status = Normal;
       sec_type = SecA;
       sec_id   = 123;
       internal_changes = [];
       cur_time = 1;
       new_packet = true;
       last_packet_header = None;
    } in
    let s = one_step(s) in 
    let s = one_step(s) in 
    let s = one_step(s) in 
    let s = one_step(s) in 
    let s = one_step(s) in 
    let s = one_step(s) in 
    let s = one_step(s) in 
    one_step(s)
;;
:testgen large_call assuming good_levels with_printer cme_test_printer_8
