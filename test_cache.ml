(** 

    Aesthetic Integration Ltd.
    Copyright 2016

    test_cache.ml

*)

open CME;;
open CME_printers;;

(** setup cache *)
let cache = [
	{
		rm_security_id = 1;
    	rm_rep_seq_num = 1;
    	rm_msg_type = V_MDUpdateAction_New;

	    rm_entry_type = V_MDEntryType_Bid; (* Bid, etc. *)
    	rm_price_level = 1;
    	rm_entry_size = 100;
    	rm_entry_px = float_to_dec(100.50);
    	rm_num_orders = 250;
    };

	{
		rm_security_id = 1;
    	rm_rep_seq_num = 2;
    	rm_msg_type = V_MDUpdateAction_New;

	    rm_entry_type = V_MDEntryType_Bid; (* Bid, etc. *)
    	rm_price_level = 1;
    	rm_entry_size = 100;
    	rm_entry_px = float_to_dec(100.50);
    	rm_num_orders = 250;
    };

	{
		rm_security_id = 1;
    	rm_rep_seq_num = 3;
    	rm_msg_type = V_MDUpdateAction_New;

	    rm_entry_type = V_MDEntryType_Bid; (* Bid, etc. *)
    	rm_price_level = 1;
    	rm_entry_size = 100;
    	rm_entry_px = float_to_dec(100.50);
    	rm_num_orders = 250;
    };

	{
		rm_security_id = 1;
    	rm_rep_seq_num = 4;
    	rm_msg_type = V_MDUpdateAction_New;

	    rm_entry_type = V_MDEntryType_Bid; (* Bid, etc. *)
    	rm_price_level = 1;
    	rm_entry_size = 100;
    	rm_entry_px = float_to_dec(100.50);
    	rm_num_orders = 250;
    };

	{
		rm_security_id = 1;
    	rm_rep_seq_num = 5;
    	rm_msg_type = V_MDUpdateAction_New;

	    rm_entry_type = V_MDEntryType_Bid; (* Bid, etc. *)
    	rm_price_level = 1;
    	rm_entry_size = 100;
    	rm_entry_px = float_to_dec(100.50);
    	rm_num_orders = 250;
    };
    
];;

(** Let's print out the cache here *)

print_string (print_cache(cache) );;
print_string "\n\n";;

(** Now let's add a message to this guy *)
let new_ref_msg = {
	rm_security_id = 1;
    rm_rep_seq_num = -1;
    rm_msg_type = V_MDUpdateAction_New;

	rm_entry_type = V_MDEntryType_Bid; (* Bid, etc. *)
    rm_price_level = 1;
    rm_entry_size = 100;
    rm_entry_px = float_to_dec(123.50);
    rm_num_orders = 250;
};;

print_string (print_cache (add_to_cache (new_ref_msg, cache)));;







