(** 
	Aesthetic Integration Ltd.

	CME_test_printer_txt.ml

   Note: Make sure you define the top-level printing function
         precisely with the following name and type -

     db_test_case_printer : exchange_state -> test_file_data list

     Where: a

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
open CME_to_json_simple;;

type test_file_data =
  {
    tf_name_prefix : string;
    tf_name_extension : string;
    tf_data : string;
  }
;;

type test_f = test_file_data list;;
(* @meta[imandra_ignore] off @end *)


let print_incoming_msg_json (s : feed_state) = 
	let next_packets = merge_messages (s.channels) in 
	string_of_packets next_packets
;;

(** is_empty *)
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
				ref_a = { r_unproc_packets = s_msgs @ s.channels.ref_a.r_unproc_packets;
						  r_proc_packets = []; }
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

(** run the simulation and generate internal state changes *)
let gen_int_messages (s : feed_state) = 
	let s' = simulate(amend_state (s)) in
	s'.internal_changes
;;

(** cme_test_printer *)
let cme_test_printer ( s : feed_state ) = 
	let int_msgs = gen_int_messages (s) in 
	let str_incoming_data = print_incoming_msg_json (s) in 
	let str_state_changes = int_messages_to_str (int_msgs) in
	[ 
		{
			tf_name_prefix = "incoming";
			tf_name_extension = "json";
			tf_data = str_incoming_data;
		};

		{
			tf_name_prefix = "state_ch";
			tf_name_extension = "txt";
			tf_data = str_state_changes;
		}
	]
;;
