(** 
	Aesthetic Integration Ltd.
	
	Copyright 2016

	http://www.cmegroup.com/confluence/display/EPICSANDBOX/MDP+3.0+-+Multiple+Depth+Book
*)

open CME;;
open CME_printers;;

(** Example from 'Modify Bid Quantity' *)
let multi_depth_book = {
	buys = [
		Level { side = BUY; qty = 100; num_orders = Some (1); price = float_to_dec (9427.50)};
		Level { side = BUY; qty = 500; num_orders = Some(19); price = float_to_dec (9427.00)};
		Level { side = BUY; qty = 750; num_orders = Some(34); price = float_to_dec (9426.50)};
		Level { side = BUY; qty = 400; num_orders = Some(25); price = float_to_dec (9425.50)}; 
	];

	sells = [ 
		Level { side = SELL; qty =  40; num_orders = Some(2); price  = float_to_dec (9427.00)};
		Level { side = SELL; qty = 600; num_orders = Some(55); price = float_to_dec (9428.00)};
		Level { side = SELL; qty = 850; num_orders = Some(55); price = float_to_dec (9429.00)};
		Level { side = SELL; qty = 350; num_orders = Some(21); price = float_to_dec (9429.50)};
		Level { side = SELL; qty = 150; num_orders = Some(12); price = float_to_dec (9430.00)};
	];
};;

let books = {
	multi = sort_book(multi_depth_book);
	implied = empty_book (5);
	combined = empty_book (0);
	book_depth = 5;
	b_status = Publishable;
};;

let msg_modify_1 = {
	rm_security_id = 1;
	rm_rep_seq_num = 1;
	rm_msg_type = V_MDUpdateAction_Change;
	rm_entry_type = V_MDEntryType_Bid;

	rm_price_level = 1;
    rm_entry_size = 1212;
    rm_entry_px = float_to_dec (9425.0);
    rm_num_orders = 100;
};; 

print_string "Example: 'Modify Bid Quantity'\n";;
print_string "The original book:\n";;
print_string (book_to_string_long (books.multi, false, books.book_depth));;

(* Now let's update the book *)
let book_1' = process_md_update_action (books, msg_modify_1);;

print_string "\n\n";;
print_string "Now we're making the change:\n";;
print_string (book_to_string_long (book_1'.multi, false, books.book_depth));;

(** Example of DeleteThru *)
let m_book_2 = {
	buys = [
		Level { side = BUY; qty = 503; num_orders = Some (20); price = float_to_dec (9427.00); };
		Level { side = BUY; qty = 750; num_orders = Some (34); price = float_to_dec (9426.50); };
		Level { side = BUY; qty = 400; num_orders = Some (25); price = float_to_dec (9426.00); };
		Level { side = BUY; qty = 300; num_orders = Some (14); price = float_to_dec (9425.50); };
		Level { side = BUY; qty = 400; num_orders = Some ( 1); price = float_to_dec (9425.00); };
	];
	sells = [
		Level { side = SELL; qty =  40; num_orders = Some (2); price = float_to_dec (9428.00); };
		Level { side = SELL; qty = 600; num_orders = Some(35); price = float_to_dec (9428.50); };
		Level { side = SELL; qty = 850; num_orders = Some(55); price = float_to_dec (9429.00); };
		Level { side = SELL; qty = 350; num_orders = Some(21); price = float_to_dec (9429.50); };
		Level { side = SELL; qty = 150; num_orders = Some(12); price = float_to_dec (9430.00); };
	];
};;

let original = {
	multi = sort_book(m_book_2);
	implied = empty_book (5);
	combined = empty_book(5);
	book_depth = 5;
	b_status = Publishable;
};;

let upd_msg_2 = {
	rm_security_id = 1;
	rm_rep_seq_num = 2;
	rm_msg_type = V_MDUpdateAction_DeleteThru;

	rm_entry_type = V_MDEntryType_Bid;
	rm_price_level = 1;
	rm_entry_size = -1;
	rm_num_orders = -1;
	rm_entry_px = -1;
};;

print_string "Example of DeleteThru\nOriginal Book:\n";;
print_string (book_to_string_long (m_book_2, false, 5));;

let books_2' = process_md_update_action (original, upd_msg_2);;

print_string "\n\nUpdated book:\n";;
print_string (one_book_to_string (books_2', Book_Type_Multi));;

let upd_msg_3 = {
	rm_security_id = 1;
	rm_rep_seq_num = 2;
	rm_msg_type = V_MDUpdateAction_Delete;

	rm_entry_type = V_MDEntryType_Offer;
	rm_price_level = 3;
	rm_entry_size = -1;
	rm_num_orders = -1;
	rm_entry_px = -1;
};;

print_string "\n\n\n\nExample of Delete\nOriginal Book:\n";;
print_string (book_to_string_long (m_book_2, false, 5));;

let books_2'' = process_md_update_action (original, upd_msg_3);;

print_string "\n\nUpdated book:\n";;
print_string (one_book_to_string (books_2'', Book_Type_Multi));;


(** New Best Price Entered *)

let upd_msg_4 = {
	rm_security_id = 1;
	rm_rep_seq_num = 2;
	rm_msg_type = V_MDUpdateAction_Change;

	rm_entry_type = V_MDEntryType_Bid;
	rm_price_level = 1;
	rm_entry_size = 200;
	rm_num_orders = 200;
	rm_entry_px = float_to_dec(9427.50);
};;	

print_string "\n\n\n\nExample of New Best Price Entered\nOriginal Book:\n";;
print_string (one_book_to_string (original, Book_Type_Multi));;

let books_3'' = process_md_update_action (original, upd_msg_4);;

print_string "\n\nUpdated book:\n";;
print_string (one_book_to_string (books_3'', Book_Type_Multi));;


(** New Order Entered at Same Price *)

let upd_msg_5 = {
	rm_security_id = 1;
	rm_rep_seq_num = 2;
	rm_msg_type = V_MDUpdateAction_Change;

	rm_entry_type = V_MDEntryType_Bid;
	rm_price_level = 1;
	rm_entry_size = 503;
	rm_num_orders = 20;
	rm_entry_px = float_to_dec(9427.00);
};;	

print_string "\n\n\n\nExample of New Order Entered at Same Price\nOriginal Book:\n";;
print_string (one_book_to_string (original, Book_Type_Multi));;

let books_4'' = process_md_update_action (original, upd_msg_5);;

print_string "\n\nUpdated book:\n";;
print_string (one_book_to_string (books_4'', Book_Type_Multi));;

(** Order Cancelled and Replaced *)

let upd_msg_6 = {
	rm_security_id = 1;
	rm_rep_seq_num = 2;
	rm_msg_type = V_MDUpdateAction_Change;

	rm_entry_type = V_MDEntryType_Bid;
	rm_price_level = 1;
	rm_entry_size = 503;
	rm_num_orders = 20;
	rm_entry_px = float_to_dec(9427.00);
};;	

print_string "\n\n\n\nExample of New Order Entered at Same Price\nOriginal Book:\n";;
print_string (one_book_to_string (original, Book_Type_Multi));;

let books_5 = process_md_update_action (original, upd_msg_5);;

print_string "\n\nUpdated book:\n";;
print_string (one_book_to_string (books_5, Book_Type_Multi));;

let upd_msg_7 = {
	rm_security_id = 1;
	rm_rep_seq_num = 2;
	rm_msg_type = V_MDUpdateAction_Change;

	rm_entry_type = V_MDEntryType_Bid;
	rm_price_level = 5;
	rm_entry_size = 400;
	rm_num_orders = 1;
	rm_entry_px = float_to_dec(9425.00);
};;	



print_string "\n\n\n\nExample of Order Cancelled and Replaced\nOriginal Book:\n";;

let m_book_8 = {
	buys = [
		Level { side = BUY; qty =  90; num_orders = Some ( 1); price = float_to_dec (9427.50); };
		Level { side = BUY; qty = 500; num_orders = Some (19); price = float_to_dec (9427.00); };
		Level { side = BUY; qty = 750; num_orders = Some (34); price = float_to_dec (9426.50); };
		Level { side = BUY; qty = 400; num_orders = Some (25); price = float_to_dec (9426.00); };
		Level { side = BUY; qty = 300; num_orders = Some (14); price = float_to_dec (9425.50); };
	];
	sells = [
		Level { side = SELL; qty =  40; num_orders = Some (2); price = float_to_dec (9428.00); };
		Level { side = SELL; qty = 600; num_orders = Some(35); price = float_to_dec (9428.50); };
		Level { side = SELL; qty = 850; num_orders = Some(55); price = float_to_dec (9429.00); };
		Level { side = SELL; qty = 350; num_orders = Some(21); price = float_to_dec (9429.50); };
		Level { side = SELL; qty = 150; num_orders = Some(12); price = float_to_dec (9430.00); };
	];
};;

let books_8 = {
	multi = sort_book (m_book_8);
	implied = empty_book(5);
	combined = empty_book(5);
	book_depth = 5;
	b_status = Publishable;
};;

let upd_msg_8 = {
	rm_security_id = 1;
	rm_rep_seq_num = 2;
	rm_msg_type = V_MDUpdateAction_Change;

	rm_entry_type = V_MDEntryType_Bid;
	rm_price_level = 5;
	rm_entry_size = 400;
	rm_num_orders = 1;
	rm_entry_px = float_to_dec(9425.00);
};;


print_string (one_book_to_string (original, Book_Type_Multi));;
let books_8' = process_md_update_action (original, upd_msg_8);;

print_string "\n\nUpdated book:\n";;
print_string (one_book_to_string (books_8', Book_Type_Multi));;







