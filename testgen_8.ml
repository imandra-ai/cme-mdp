:load CME.ml

:load_ocaml CME_printers.ml
:load_ocaml CME_test_helper.ml
:load_ocaml CME_to_json_simple.ml
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

let large_call (s, msg1, msg2, msg3, msg4, msg5, msg6, msg7, msg8) =
    let s' = { s with
           books = { book_depth = 5; multi = {buys = []; sells = []};
         implied = {buys = []; sells = []};
        combined = {buys = []; sells = []};
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
:testgen large_call with_printer cme_test_printer_8
