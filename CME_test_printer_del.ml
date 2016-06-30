
(** 
	Aesthetic Integration Ltd.

	CME_test_printer_txt.ml

   Note: Make sure you define the top-level printing function
         precisely with the following name and type -

     db_test_case_printer : exchange_state -> test_file_data list

     Where:
     type test_file_data =
      {
        tf_name_prefix : string;
        tf_name_extension : string;
        tf_data : string;
      }
 *)

(* @meta[imandra_ignore] on @end *)
open CME;;
open CME_test_helper;;
open CME_printers;;
open CME_json;;

type test_file_data =
  {
    tf_name_prefix : string;
    tf_name_extension : string;
    tf_data : string;
  }
;;

type test_f = test_file_data list;;
(* @meta[imandra_ignore] off @end *)

type ip_addresses = {
	ia_ref_a : string;
	ia_ref_b : string;
	ia_snap_a : string;
	ia_snap_b : string;
};;

(** This should be configurable *)
let addresses = {
	ia_ref_a = "127.0.0.1:1";
	ia_ref_b = "127.0.0.1:2";
	ia_snap_a = "127.0.0.1:3";
	ia_snap_b = "127.0.0.1:4";
};;


let print_incoming_msg_json (s : feed_state) = 
	let next_packets = merge_messages (s.channels) in 
	packets_to_string next_packets
;;

let is_empty ( ch : channels ) =
	ch.ref_a.r_unproc_packets = [] &&
	ch.ref_b.r_unproc_packets = [] &&
	ch.snap_a.s_unproc_packets = [] &&
	ch.snap_b.s_unproc_packets = []
;;

(** Generate internal messages for the state of the feed *)
let amend_state (s : feed_state) = 
	let setup_msgs = book_to_messages(s) in 
	let s_msgs = explode_ref_packets (setup_msgs, []) in 
	let b = s.books in 
	{
		s with
			channels = {
				s.channels with
				ref_a = 
					{ 
						r_unproc_packets = s_msgs @ s.channels.ref_a.r_unproc_packets;
						r_proc_packets = [];
					}
			};

			books = {
				book_depth = b.book_depth;
				multi = { buys = []; sells = []; };
				implied = { buys = []; sells = []; };
				combined = { buys = []; sells = []; };
				b_status = Empty;
			};
	}
;;

(** Run the simulation and generate internal state changes *)
let gen_int_messages (s : feed_state) = 
	let s' = simulate(amend_state (s)) in
	s'.internal_changes
;;


(** The actual test case printer *)
let cme_test_printer ( s : feed_state ) = 
	let setup_msgs = book_to_messages(s) in 
	let s_msgs = explode_ref_packets (setup_msgs, []) in

	let str_incoming_data_a_ref = packets_to_string (create_packets_ref (s_msgs @ s.channels.ref_a.r_unproc_packets)) in 
	let str_incoming_data_b_ref = packets_to_string (create_packets_ref (s.channels.ref_b.r_unproc_packets)) in 

	let str_incoming_data_a_snap = packets_to_string (create_packets_snap (s.channels.snap_a.s_unproc_packets)) in 
	let str_incoming_data_b_snap = packets_to_string (create_packets_snap (s.channels.snap_b.s_unproc_packets)) in 

	let str_state_changes = int_messages_to_str_format (gen_int_messages (s)) in
	[ 
		{
			tf_name_prefix = addresses.ia_ref_a ^ "_ref_a";
			tf_name_extension = "json";
			tf_data = str_incoming_data_a_ref;
		};

		{
			tf_name_prefix = addresses.ia_ref_b ^ "_ref_b";
			tf_name_extension = "json";
			tf_data = str_incoming_data_b_ref;
		};

		{
			tf_name_prefix = addresses.ia_snap_a ^ "_snap_a";
			tf_name_extension = "json";
			tf_data = str_incoming_data_a_snap;
		};

		{
			tf_name_prefix = addresses.ia_snap_b ^ "_snap_b";
			tf_name_extension = "json";
			tf_data = str_incoming_data_b_snap;
		};

		{
			tf_name_prefix = "state_ch";
			tf_name_extension = "txt";
			tf_data = str_state_changes;
		}
	]
;;

(** The wrapper to take into account the messages *)
let cme_test_printer_8 (s, msg1, msg2, msg3, msg4, msg5, msg6, msg7, msg8) = 
	let s' = {
			s with 
      		channels = { 
        		s.channels with ref_a = { s.channels.ref_a with r_unproc_packets = [msg1; msg2; msg3] };
                        ref_b = { s.channels.ref_b with r_unproc_packets = [msg4; msg5] };
                        snap_a = { s.channels.snap_a with s_unproc_packets = [msg6; msg7; msg8] };
                        snap_b = { s.channels.snap_b with s_unproc_packets = [] };
      		}
  		} in 
	cme_test_printer (s')
;;


(** The wrapper to take into account the messages *)
let cme_test_printer_3 (s, msg1, msg2, msg3) = 
	let s' = {
			s with 
      		channels = { 
        		s.channels with ref_a = { s.channels.ref_a with r_unproc_packets = [msg1; msg2; msg3] };
                        ref_b = { s.channels.ref_b with r_unproc_packets = [] };
                        snap_a = { s.channels.snap_a with s_unproc_packets = [] };
                        snap_b = { s.channels.snap_b with s_unproc_packets = [] };
      		}
  		} in 
	cme_test_printer (s')
;;
  	
