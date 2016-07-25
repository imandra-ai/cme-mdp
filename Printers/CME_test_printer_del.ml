
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

(**  Extract the right messages from the packet stream. TODO: should probably a better way to do this. *)
let rec get_a_ref ( packets ) = 
	match packets with 
	| [] -> []
	| x::xs -> 
		match x.packet_channel with
		| Ch_Ref_A -> x :: get_a_ref (xs)
		| _ -> get_a_ref(xs)
;;

let rec get_b_ref ( packets ) = 
	match packets with 
	| [] -> []
	| x :: xs ->
		match x.packet_channel with 
		| Ch_Ref_B -> x :: get_b_ref (xs)
		| _ -> get_b_ref (xs)
;;

let rec get_snap_a ( packets ) = 
	match packets with 
	| [] -> []
	| x :: xs ->
		match x.packet_channel with
		| Ch_Snap_A -> x :: get_snap_a (xs)
		| _ -> get_snap_a (xs)
;;

let rec get_snap_b ( packets ) =
	match packets with 
	| [] -> []
	| x :: xs -> 
		match x.packet_channel with 
		| Ch_Snap_B -> x :: get_snap_b (xs)
		| _ -> get_snap_b (xs)
;;

(** Run the simulation of the feed model from here *)
let get_feed_state_changes (packets) =
	(** Note that we should be  *)
	let s = { init_feed_state with 
				unprocessed_packets = packets; } in 
	
	let s' = simulate (s) in 
;; 

(** New printer for the CME model *)
let cme_new_test_printer ( msg1, msg2, msg3, msg4, msg5, msg6, msg7, msg8 ) = 

	(** We need to accept individual messages, so we compose a list out of them here *)
	let int_m_list = [ msg1; msg2; msg3; msg4; msg5; msg6; msg7; msg8] in 

	(** Now we run the simulation of the exchange to generate all of the  *)
	let proc_exchange_state = simulate_exchange (init_ex_state, int_m_list) in 
	
	(** proc_exchange_state should now contains all of the packets that the Exchanges generates
		so here we extract the individual channels so we can later save them in binary format *)
	let str_incoming_data_a_ref = get_a_ref (proc_exchange_state.packets) in 
	let str_incoming_data_b_ref = get_b_ref (proc_echange_state.packets) in 
	let str_incoming_data_a_snap = get_a_snap (proc_exchange_state.packets) in 
	let str_incoming_data_b_snap = get_b_snap (proc_exchange_state.packets) in 

	(** The last step is to run simulation on the feed using the individual channel data so 
		we get a list of internal state transitions *)
	let str_state_changes = int_messages_to_str_format (get_int_messages (proc_exchange_state.packets)) in 
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
			tf_data = str_incoming_data_snap_a;
		};

		{
			tf_name_prefix = addresses._ia_snap_b ^ "_snap_b";
			tf_name_extension = "json";
			tf_data = str_incoming_data_snap_b;
		};
		{

			tf_name_prefix = "state_ch";
			tf_name_extension = "txt";
			tf_data = str_state_changes;
		};
	]


;;

