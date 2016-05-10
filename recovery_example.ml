(** 
	
	Aesthetic Integration Ltd.

	Copyright 2016

*)

open CME;;
open CME_printers;;

(** initial books *)
let init_books = {
	multi = empty_book (5);
	implied = empty_book (5);
	combined = empty_book (5);
	book_depth = 5;
	b_status = Empty;
};;

(** The scenario is as follows:
	--> We receive a number of packets to build out the book
	--> There's a gap
	--> Then we start processing the snapshots and recover the book
*)

(** Incremental refresh packets for channel A *)
let inc_packets_a = [
	IncRefreshPacket {
		rp_header = {
			ph_packet_seq_num = 1; 
			ph_sending_time = 1; 
		};

		rp_msg = {
		    rm_security_id = 2;
		    rm_rep_seq_num = 1;
		    rm_msg_type = V_MDUpdateAction_Change;

		    rm_entry_type = V_MDEntryType_Bid;
		    rm_price_level = 1;
		    rm_entry_size = 500;
		    rm_entry_px = float_to_dec (100.50);
		    rm_num_orders = 5;
		};
	};

	IncRefreshPacket {
		rp_header = { 
			ph_packet_seq_num = 1;
			ph_sending_time = 1;
		};

		rp_msg = {
		    rm_security_id = 2;
		    rm_rep_seq_num = 2;
		    rm_msg_type = V_MDUpdateAction_Change;

		    rm_entry_type = V_MDEntryType_Offer;
		    rm_price_level = 1;
		    rm_entry_size = 1500;
		    rm_entry_px = float_to_dec (101.50);
		    rm_num_orders = 15;
		};
	};

	(** The second packet *)
	IncRefreshPacket {
		rp_header = { 
			ph_packet_seq_num = 2;
			ph_sending_time = 2;
		};

		rp_msg = {
		    rm_security_id = 2;
		    rm_rep_seq_num = 3;
		    rm_msg_type = V_MDUpdateAction_Change;

		    rm_entry_type = V_MDEntryType_Bid;
		    rm_price_level = 2;
		    rm_entry_size = 1500;
		    rm_entry_px = float_to_dec (100.00);
		    rm_num_orders = 15;
		};
	};

	IncRefreshPacket {
		rp_header = { 
			ph_packet_seq_num = 2;
			ph_sending_time = 2;
		};

		rp_msg = {
		    rm_security_id = 2;
		    rm_rep_seq_num = 4;
		    rm_msg_type = V_MDUpdateAction_Change;

		    rm_entry_type = V_MDEntryType_Offer;
		    rm_price_level = 2;
		    rm_entry_size = 1500;
		    rm_entry_px = float_to_dec (102.00);
		    rm_num_orders = 15;
		};
	};
];; 

(** Incremental refresh packets for channel B *)
let inc_packets_b = [
	IncRefreshPacket {
		rp_header = {
			ph_packet_seq_num = 1; 
			ph_sending_time = 1; 
		};

		rp_msg = {
		    rm_security_id = 2;
		    rm_rep_seq_num = 1;
		    rm_msg_type = V_MDUpdateAction_Change;

		    rm_entry_type = V_MDEntryType_Bid;
		    rm_price_level = 1;
		    rm_entry_size = 500;
		    rm_entry_px = float_to_dec (100.50);
		    rm_num_orders = 5;
		};
	};

	IncRefreshPacket {
		rp_header = { 
			ph_packet_seq_num = 1;
			ph_sending_time = 1;
		};

		rp_msg = {
		    rm_security_id = 2;
		    rm_rep_seq_num = 2;
		    rm_msg_type = V_MDUpdateAction_Change;

		    rm_entry_type = V_MDEntryType_Offer;
		    rm_price_level = 1;
		    rm_entry_size = 1500;
		    rm_entry_px = float_to_dec (101.50);
		    rm_num_orders = 15;
		};
	};

	(** The second packet *)
	IncRefreshPacket {
		rp_header = { 
			ph_packet_seq_num = 2;
			ph_sending_time = 2;
		};

		rp_msg = {
		    rm_security_id = 2;
		    rm_rep_seq_num = 3;
		    rm_msg_type = V_MDUpdateAction_Change;

		    rm_entry_type = V_MDEntryType_Bid;
		    rm_price_level = 2;
		    rm_entry_size = 1500;
		    rm_entry_px = float_to_dec (100.00);
		    rm_num_orders = 15;
		};
	};

	IncRefreshPacket {
		rp_header = { 
			ph_packet_seq_num = 2;
			ph_sending_time = 2;
		};

		rp_msg = {
		    rm_security_id = 2;
		    rm_rep_seq_num = 4;
		    rm_msg_type = V_MDUpdateAction_Change;

		    rm_entry_type = V_MDEntryType_Offer;
		    rm_price_level = 2;
		    rm_entry_size = 1500;
		    rm_entry_px = float_to_dec (102.00);
		    rm_num_orders = 15;
		};
	};
];;

(** Snapshot packets *)
let ref_packets_a = [
	SnapshotPacket {
		sp_header = { 
			ph_packet_seq_num = 4;
			ph_sending_time = 1;
		};
		sp_snap = {
		    sm_security_id = 2;
		    sm_last_msg_seq_num_processed = 2;
		    sm_rep_seq_num = 2;

		    sm_real_bid = Level { side = BUY; 	qty = 1; price = float_to_dec (100.00); num_orders = Some (200); };
		    sm_real_ask = Level { side = SELL; 	qty = 1; price = float_to_dec (100.00); num_orders = Some (300); };
		    sm_imp_bid 	= Level { side = BUY; 	qty = 1; price = float_to_dec (150.00); num_orders = Some (400); };
		    sm_imp_ask 	= Level { side = SELL; 	qty = 1; price = float_to_dec (150.00); num_orders = Some (400); };
		};
	};

	SnapshotPacket {
		sp_header = {
			ph_packet_seq_num = 5;
			ph_sending_time = 5;
		};
		sp_snap = {
		    sm_security_id = 2;
		    sm_last_msg_seq_num_processed = 2;    
		    sm_rep_seq_num = 2;              

		    sm_real_bid = Level { side = BUY; 	qty = 1; price = float_to_dec (100.50); num_orders = Some (100); };
		    sm_real_ask = Level { side = SELL; 	qty = 1; price = float_to_dec (100.00); num_orders = Some (200); };
		    sm_imp_bid 	= Level { side = BUY; 	qty = 1; price = float_to_dec (100.00); num_orders = Some (300); };
		    sm_imp_ask 	= Level { side = SELL; 	qty = 1; price = float_to_dec (150.00); num_orders = Some (400); };
		};
	};
	
	SnapshotPacket {
		sp_header = { 
			ph_packet_seq_num = 6;
			ph_sending_time = 6;
		};
		sp_snap = {
		    sm_security_id = 3;
		    sm_last_msg_seq_num_processed = 3;
		    sm_rep_seq_num = 3;

		    sm_real_bid = Level { side = BUY;	qty = 1; price = float_to_dec (100.50); num_orders = Some (100); };
		    sm_real_ask = Level { side = SELL; 	qty = 1; price = float_to_dec (100.50); num_orders = Some (100); };
		    sm_imp_bid 	= Level { side = BUY; 	qty = 1; price = float_to_dec (105.00); num_orders = Some (100); };
		    sm_imp_ask 	= Level { side = SELL; 	qty = 1; price = float_to_dec (110.00); num_orders = Some (100); };
		};
	};
];;

(** initial feed state *)
let init_feed_state = {
	channels = {
		inc_a = {
			unproc_packets = inc_packets_a;
			proc_packets = [];
		};

		inc_b = {
			unproc_packets = inc_packets_b;
			proc_packets = [];
		};

		last_seq_processed = 0;
		
		cache = [ ];
		
		last_snapshot = None;
		
		ref_a = {
			unproc_packets = ref_packets_a;
			proc_packets = [];
		};

		cycle_hist_a = {
    		reference_sec_id = 1;
    		self_sec_id = 2;
    		ref_sec_snap_received = false;
    		liq = LiqUnknown;
		};
		
		ref_b = {
			unproc_packets = [];
			proc_packets = [];
		};
		
		cycle_hist_b = {
    		reference_sec_id = 1;
		    self_sec_id = 2;
    		ref_sec_snap_received = false;
    		liq = LiqUnknown;
		};
	};

	sec_type = FUTURES;	
	sec_id = 2;
	books = init_books;
	feed_status = Normal;
	internal_changes = [];
	cur_time = 0;
};;

(** Run through the simulation and generate the internal state change messages *)
let s' = simulate(init_feed_state);;

let print_state_info (s : feed_state) = 
	let msgs = print_state_messages (s.internal_changes) in 
	Printf.sprintf "Print the internal messages:\n%s" msgs
;;

print_string (print_state_info (s'));;
