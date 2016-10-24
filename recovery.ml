#use "topfind";;
#require "str";;
#require "unix";;
#require "yojson";;

:load Model/CME_Types.ml
:load Model/CME.ml
:load Model/CME_Exchange.ml
:load_ocaml Printers/CME_json.ml
:load_ocaml Printers/CME_Exchange_json.ml
:load_ocaml Printers/CME_Internal_json.ml

:load c835d_153.ml

let state = {
	feed_sec_type = SecA;
	feed_sec_id   = 1; (* TODO Fix the secid shizm*)

	(* The books that we're maintaining, etc... *)
	books = {
		book_depth = 5;
		multi      = empty_book 5;
		implied    = empty_book 5;
		combined   = empty_book 5;
		b_status   = Empty
	};

	channels = {    
		unprocessed_packets = List.rev starting_state.pac_queue;

		processed_messages = [];
		processed_ref_a = [];
		processed_ref_b = [];
		processed_snap_a = [];
		processed_snap_b = [];

		cycle_hist_a = clean_cycle_hist;      
		cycle_hist_b = clean_cycle_hist;

		last_seq_processed = 0;
		cache = [];       

		last_snapshot = None
	};

	feed_status = Normal;
	internal_changes = [];
	cur_time = 0;
};;
