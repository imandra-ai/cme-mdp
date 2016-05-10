(** A couple of examples of book building/modification *)

open CME;;
open CME_printers;;

(** Example of combining a multi-depth book and an implied book

	See this: 
	http://www.cmegroup.com/confluence/display/EPICSANDBOX/MDP+3.0+-+Consolidating+Implied+and+Multiple+Depth+Books
*)


(** multi-depth book *)
let book_m_2 = {
	buys = [
		Level { side = BUY; qty = 500; num_orders = Some (19);  price = float_to_dec (9427.00); };
		Level { side = BUY; qty = 750; num_orders = Some (34);  price = float_to_dec (9426.50); };
		Level { side = BUY; qty = 400; num_orders = Some (25);  price = float_to_dec (9426.00); };
		Level { side = BUY; qty = 300; num_orders = Some (14);  price = float_to_dec (9425.50); };
	 (* Level { side = BUY; qty = 200; num_orders = Some (10);  price = float_to_dec (9425.00); }; *)
	];
	sells = [
		Level { side = SELL; qty = 40;  num_orders = Some (2);  price = float_to_dec (9428.00); };
		Level { side = SELL; qty = 600; num_orders = Some (35); price = float_to_dec (9428.50); };
		Level { side = SELL; qty = 850; num_orders = Some (55); price = float_to_dec (9429.00); };
		Level { side = SELL; qty = 350; num_orders = Some (21); price = float_to_dec (9429.50); };
		Level { side = SELL; qty = 150; num_orders = Some (1);  price = float_to_dec (9430.50); };
	];
};;


(** implied book *)
let book_i_2 = {
	buys = [
		Level { side = BUY; qty = 100; num_orders = None; price = float_to_dec (9427.50);};
		Level { side = BUY; qty = 200; num_orders = None; price = float_to_dec (9427.00);};
	];
	sells = [
		Level { side = SELL; qty = 40; 	num_orders = None; price = float_to_dec (9428.00)};
		Level { side = SELL; qty = 100; num_orders = None; price = float_to_dec (9430.00)};
	];
};;

let books_2 = {
	implied = book_i_2;
	multi = book_m_2;
	combined = empty_book(1);
	book_depth = 5;
	b_status = Publishable;
};;

let books_2' = recalc_combined (books_2);;

print_string (books_to_string (books_2'));;

(** 
	Example of implied book updates
	See this: http://www.cmegroup.com/confluence/display/EPICSANDBOX/MDP+3.0+-+Implied+Book
*)
let book_i_3 = {
	buys = [
		Level { side = BUY; qty = 100; num_orders = None; price = float_to_dec (9427.50); };
		Level { side = BUY; qty = 200; num_orders = None; price = float_to_dec (9427.00); };
	];
	sells = [
		Level { side = SELL; qty = 40; num_orders = None; price = float_to_dec (9428.00); };
		Level { side = SELL; qty = 100; num_orders= None; price = float_to_dec (9428.50); };
	];
};;

let books_3 = {
	multi = empty_book (5);
	implied = book_i_3;
	combined = empty_book(5);
	book_depth = 5;
	b_status = Publishable;
};;

let ref_msg_3 = { 
	rm_security_id = 1;
    rm_rep_seq_num = 1;
    rm_msg_type = V_MDUpdateAction_Change;

    rm_entry_type = V_MDEntryType_ImpliedBid;
    rm_price_level = 2;
    rm_entry_size = 90;
    rm_entry_px = float_to_dec (9427.00);
    rm_num_orders = 100; (** This is not required, but we put this here anyway *)
};;

print_string "Example 3:\n\n";;
print_string "Original implied book:\n";;
print_string (one_book_to_string (books_3, Book_Type_Implied));;

let books_3' = process_md_update_action (books_3, ref_msg_3);;

print_string "\nUpdated implied book:\n";;
print_string (one_book_to_string (books_3', Book_Type_Implied));;

let ref_msg_4 = {
	rm_security_id = 1;
	rm_rep_seq_num = 1;
	rm_msg_type = V_MDUpdateAction_Delete;

	rm_entry_type = V_MDEntryType_ImpliedBid;
    rm_price_level = 1;
    rm_entry_size = 100;
    rm_entry_px = float_to_dec (9427.50);
    rm_num_orders = 100;
};;

let ref_msg_5 = {
	rm_security_id = 1;
	rm_rep_seq_num = 1;
	rm_msg_type = V_MDUpdateAction_New;

	rm_entry_type = V_MDEntryType_ImpliedBid;
    rm_price_level = 2;
    rm_entry_size = 80;
    rm_entry_px = float_to_dec (9426.50);
    rm_num_orders = 100;
};;

let books_4 = process_md_update_action (process_md_update_action (books_3', ref_msg_4), ref_msg_5);;

print_string "\nUpdated implied book:\n";;
print_string (one_book_to_string (books_4, Book_Type_Implied));;

let ref_msg_6 = {
	rm_security_id = 1;
	rm_rep_seq_num = 1;
	rm_msg_type = V_MDUpdateAction_Change;

	rm_entry_type = V_MDEntryType_ImpliedBid;
    rm_price_level = 1;
    rm_entry_size = 93;
    rm_entry_px = float_to_dec (9427.00);
    rm_num_orders = 100;
};;

let books_5 = process_md_update_action (books_4, ref_msg_6);;

print_string "\nUpdated implied book:\n";;
print_string (one_book_to_string (books_5, Book_Type_Implied));;



