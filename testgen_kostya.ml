:load CME.ml

:load_ocaml CME_printers.ml
:load_ocaml CME_test_helper.ml
:load_ocaml kojson.ml
:load_ocaml CME_json.ml
:load_ocaml CME_test_printer_del.ml

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

let large_call_3 (s, msg1, msg2, msg3) =
    let s' = { 
        books = { 
            book_depth = 5; multi = {buys = []; sells = []};
            implied = {buys = []; sells = []};
            combined = {buys = []; sells = []};
            b_status = Publishable; 
        };
        channels = {  
            ref_a  = { s.channels.ref_a with r_unproc_packets = [msg1; msg2; msg3]; };
            ref_b  = { s.channels.ref_b with r_unproc_packets = []        };
            snap_a = { s.channels.snap_a with s_unproc_packets = [] };
            snap_b = { s.channels.snap_b with s_unproc_packets = [] };
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
       sec_type = FUTURES;
       sec_id   = 123;
       internal_changes = [];
       cur_time = 1;
       new_packet = true;
       last_packet_header = None;
    } in
 let s' = one_step(s') in 
 let s' = one_step(s') in 
 let s' = one_step(s') in 
 one_step(s')
;;

:testgen large_call_3 with_printer cme_test_printer_3
