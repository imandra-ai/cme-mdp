(** 

	Test printers 

*)

open CME;;
open CME_printers;;
open CME_test_printer_txt;;

let x1 = { 
	sec_type = FUTURES;
    sec_id = 8;
    books = { 
    		book_depth = 2;
                multi = {
            	    buys = [];
                  sells = []; };
          
                implied = {
                	buys = [];
                  sells = []; 
                };
          
               	combined = {
               		buys = [];
                    sells = [];
                };
                   b_status = Empty; };
                   channels = { ref_a = { r_unproc_packets = [ { rp_header = { ph_packet_seq_num = 14;
                                                                                   ph_sending_time = 15; };
                                                                     rp_msg = { rm_security_id = 16;
                                                                                rm_rep_seq_num = 1;
                                                                                rm_msg_type = V_MDUpdateAction_New;
                                                                                rm_entry_type = V_MDEntryType_Bid;
                                                                                rm_price_level = 17;
                                                                                rm_entry_size = 18;
                                                                                rm_entry_px = 19;
                                                                                rm_num_orders = 20; }; }];
                                r_proc_packets = []; };
                      ref_b = { r_unproc_packets = [ ];
                                r_proc_packets = []; };
                      last_seq_processed = 7;
                      cache = [];
                      last_snapshot = None;
                      snap_a = { s_unproc_packets = [ { sp_header = { ph_packet_seq_num = 22;
                                                                                 ph_sending_time = 23; };
                                                                   sp_snap = { sm_security_id = 24;
                                                                               sm_last_msg_seq_num_processed = (-1);
                                                                               sm_rep_seq_num = 25;
                                                                               sm_real_bid = NoLevel;
                                                                               sm_real_ask = NoLevel;
                                                                               sm_imp_bid = NoLevel;
                                                                               sm_imp_ask = NoLevel; }; }];
                                s_proc_packets = []; };
                      cycle_hist_a = { reference_sec_id = 3;
                                       self_sec_id = 4;
                                       ref_sec_snap_received = true;
                                       liq = Liquid; };

                      snap_b = { s_unproc_packets = [];
                                s_proc_packets = []; };
                      cycle_hist_b = { reference_sec_id = 5;
                                       self_sec_id = 6;
                                       ref_sec_snap_received = false;
                                       liq = Liquid; }; };
         feed_status = InRecovery;
         internal_changes = [];
         cur_time = 9;
};;

let x2 = { sec_type = FUTURES;
         sec_id = 9;
         books = { book_depth = 3;
                   multi = { buys = [];
                             sells = []; };
                   implied = { buys = [];
                               sells = []; };
                   combined = { buys = [];
                                sells = []; };
                   b_status = Empty; };
         channels = { ref_a = { 
                      r_unproc_packets = [{ rp_header = { ph_packet_seq_num = 15;
                                                          ph_sending_time = 16; };
                                                          rp_msg = {  rm_security_id = 17;
                                                                      rm_rep_seq_num = 2;
                                                                      rm_msg_type = V_MDUpdateAction_New;
                                                                      rm_entry_type = V_MDEntryType_Bid;
                                                                      rm_price_level = 18;
                                                                      rm_entry_size = 19;
                                                                      rm_entry_px = 20;
                                                                      rm_num_orders = 21; }; }];
                      r_proc_packets = []; };
                      ref_b = { r_unproc_packets = [];
                                r_proc_packets = []; };
                      last_seq_processed = 8;
                      cache = [];
                      last_snapshot = Some ({ snap_m_book = { buys = [];
                                                              sells = []; };
                                              snap_i_book = { buys = [];
                                                              sells = []; };
                                              snap_seq_num = (-1); });

                      snap_a = { s_unproc_packets = [ { sp_header = { ph_packet_seq_num = 23;
                                                                                 ph_sending_time = 24; };
                                                                   sp_snap = { sm_security_id = 25;
                                                                               sm_last_msg_seq_num_processed = 0;
                                                                               sm_rep_seq_num = 26;
                                                                               sm_real_bid = NoLevel;
                                                                               sm_real_ask = NoLevel;
                                                                               sm_imp_bid = NoLevel;
                                                                               sm_imp_ask = NoLevel; }; }];
                                s_proc_packets = []; };
                      cycle_hist_a = { reference_sec_id = 4;
                                       self_sec_id = 5;
                                       ref_sec_snap_received = true;
                                       liq = Liquid; };
                      snap_b = {  s_unproc_packets = [];
                                  s_proc_packets = []; };
                      cycle_hist_b = { reference_sec_id = 6;
                                       self_sec_id = 7;
                                       ref_sec_snap_received = false;
                                       liq = Liquid; }; };
         feed_status = InRecovery;
         internal_changes = [];
         cur_time = 10;
};;

let x3 = { sec_type = FUTURES;
         sec_id = 9;
         books = { book_depth = 3;
                   multi = { buys = [];
                             sells = []; };
                   implied = { buys = [];
                               sells = []; };
                   combined = { buys = [];
                                sells = []; };
                   b_status = Empty; };
         channels = { ref_a = { r_unproc_packets = [{ rp_header = { ph_packet_seq_num = 15;
                                                                                   ph_sending_time = 16; };
                                                                     rp_msg = { rm_security_id = 17;
                                                                                rm_rep_seq_num = 2;
                                                                                rm_msg_type = V_MDUpdateAction_New;
                                                                                rm_entry_type = V_MDEntryType_Bid;
                                                                                rm_price_level = 18;
                                                                                rm_entry_size = 19;
                                                                                rm_entry_px = 20;
                                                                                rm_num_orders = 21; }; }];
                                r_proc_packets = []; };
                      ref_b = { r_unproc_packets = [];
                                r_proc_packets = []; };
                      last_seq_processed = 8;
                      cache = [];
                      last_snapshot = Some ({ snap_m_book = { buys = [];
                                                              sells = []; };
                                              snap_i_book = { buys = [];
                                                              sells = []; };
                                              snap_seq_num = (-1); });
                      snap_a = { s_unproc_packets = [ { sp_header = { ph_packet_seq_num = 23;
                                                                                 ph_sending_time = 24; };
                                                                   sp_snap = { sm_security_id = 25;
                                                                               sm_last_msg_seq_num_processed = 0;
                                                                               sm_rep_seq_num = 26;
                                                                               sm_real_bid = NoLevel;
                                                                               sm_real_ask = NoLevel;
                                                                               sm_imp_bid = NoLevel;
                                                                               sm_imp_ask = NoLevel; }; }];
                                s_proc_packets = []; };
                      cycle_hist_a = { reference_sec_id = 4;
                                       self_sec_id = 5;
                                       ref_sec_snap_received = false;
                                       liq = Liquid; };
                      snap_b = {  s_unproc_packets = [];
                                  s_proc_packets = []; };
                      cycle_hist_b = { reference_sec_id = 6;
                                       self_sec_id = 7;
                                       ref_sec_snap_received = false;
                                       liq = Liquid; }; };
         feed_status = InRecovery;
         internal_changes = [];
         cur_time = 10;
};;

print_string "Example 1\n";;
let test1 = cme_test_printer (x1);;

print_string "Printing out the Incoming Messages:\n";;
print_string ((List.hd test1).tf_data);;

print_string "\n\nPrinting out the Internal State Changes:\n";;
print_string ((List.hd (List.tl test1)).tf_data);;


print_string "Example 2\n";;
let test2 = cme_test_printer (x2);;

print_string "Printing out the Incoming Messages:\n";;
print_string ((List.hd test2).tf_data);;

print_string "\n\nPrinting out the Internal State Changes:\n";;
print_string ((List.hd (List.tl test2)).tf_data);;


print_string "Example 3\n";;
let test3 = cme_test_printer (x3);;

print_string "Printing out the Incoming Messages:\n";;
print_string ((List.hd test3).tf_data);;

print_string "\n\nPrinting out the Internal State Changes:\n";;
print_string ((List.hd (List.tl test3)).tf_data);;




