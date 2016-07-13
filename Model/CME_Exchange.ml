(**
    Aesthetic Integration Ltd.
    Copyright 2016

    CME_Exchange.ml
*)


(** 1. Internal state *)

(** 1.1 Books.  *)
(** We will just hard-code the book levels here *)
type book_side = {
    one   : order_level;
    two   : order_level;
    three : order_level;
    four  : order_level;
    five  : order_level;
};;

type order_book = {
     buy_orders : book_side;
    sell_orders : book_side;
};;

(** 1.1.1 Book access function: get level  *)
let get_obook_level (bs, level_num : book_side * int) = 
    match level_num with 
    | 1 -> bs.one
    | 2 -> bs.two
    | 3 -> bs.three
    | 4 -> bs.four
    | _ -> bs.five
;;

(** 1.2 Security state including the order book         *)
type security_state = {
    last_rep_seq_num : int;     (* Last RepSeqNum for the sc*)
    sec_id : int;               (* Security ID              *)
    multi_book : order_book;    (* Multi depth book         *)
    implied_book : order_book;  (* Implied book             *)
};;

(** 1.3 The state of the whole exchange.                  *)
type exchange_state = {
    sec_a : security_state;     (* Security A orderbook     *)
    sec_b : security_state;	    (* Security B orderbook     *)

    (** Queue of events that have occured since the         *)
    msg_queue : message list;

    (** Packets queue                                       *)
    pac_queue: packet list;
    p_seq_num : int;            (* Packet sequence number   *)
};;

(** 1.3.1 State access/modfication utility functions *)

let get_security_id ( state, security ) = 
    match security with 
    | SecA -> state.sec_a.sec_id
    | SecB -> state.sec_b.sec_id
;;

let get_rep_seq_num ( state, security ) = 
    match security with 
    | SecA -> state.sec_a.last_rep_seq_num
    | SecB -> state.sec_b.last_rep_seq_num
;;

let get_level ( state, security, book_type, order_side, nlevel  : exchange_state * sec_type * book_type * order_side * int ) =
    let s_state = match security with | SecA -> state.sec_a | SecB -> state.sec_b in
    match book_type, order_side with 
        | Book_Type_Multi    , OrdBuy  -> get_obook_level ( s_state.multi_book.buy_orders    , nlevel )
        | Book_Type_Implied  , OrdBuy  -> get_obook_level ( s_state.implied_book.buy_orders  , nlevel )
        | Book_Type_Multi    , OrdSell -> get_obook_level ( s_state.multi_book.sell_orders   , nlevel )
        | Book_Type_Implied  , OrdSell -> get_obook_level ( s_state.implied_book.sell_orders , nlevel )
        | Book_Type_Combined ,  _ -> None 
;;

let advance_rep_seq_num ( state, sec_type ) =
    match sec_type with 
    | SecA -> { state with sec_a = {state.sec_a with last_rep_seq_num = state.sec_a.last_rep_seq_num } }
    | SecB -> { state with sec_b = {state.sec_b with last_rep_seq_num = state.sec_b.last_rep_seq_num } }
;;


(** 2. Internal events *)

(** 2.1 Add order internal event *)
type ord_add_data = {
    oa_order_qty : int;
    oa_price : int;

    oa_sec_type : sec_type;
    oa_book_type : book_type;
    oa_level_num : int;
    oa_level_side : order_side;
    oa_num_orders : int option
};;

(** 2.1.1 Send a new level command *)
let send_add_level (state, o_add) = 
    let state = advance_rep_seq_num ( state, o_add.oa_sec_type ) in
    let add_m = RefreshMessage {
        rm_security_id = get_security_id ( state, o_add.oa_sec_type ) ;
        rm_rep_seq_num = get_rep_seq_num ( state, o_add.oa_sec_type ) ;
        rm_msg_type    = V_MDUpdateAction_New ;

        rm_entry_type  = side_to_entry_type ( o_add.oa_book_type, o_add.oa_level_side ) ; 
        rm_entry_px    = o_add.oa_price;
        rm_price_level = o_add.oa_level_num;
        rm_entry_size  = o_add.oa_order_qty;
        rm_num_orders  = o_add.oa_num_orders
    } in 
    { state with msg_queue = add_m :: state.msg_queue; }
;;


(** 2.2 Order Change internal event *)
(**     - note that it can only change qty -- not num_orders or price... *)
(**     - TODO -- add each entry being optional *) 
type ord_change_data = {
    oc_level_side : order_side;
    oc_new_qty : int;

    oc_sec_type : sec_type;
    oc_book_type : book_type;
    oc_level_num : int;
};;

(** 2.2.1 Send change of level command *)
let send_o_change (state, o_change) = 
    let state = advance_rep_seq_num ( state, o_change.oc_sec_type ) in
    let level = get_level (state , o_change.oc_sec_type, o_change.oc_book_type, o_change.oc_level_side, o_change.oc_level_num ) in
    match level with None -> state | Some level -> 
    let change_m = RefreshMessage {
        rm_security_id = get_security_id ( state, o_change.oc_sec_type ) ;
        rm_rep_seq_num = get_rep_seq_num ( state, o_change.oc_sec_type ) ;
        rm_msg_type    = V_MDUpdateAction_Change ;

        rm_entry_type  = side_to_entry_type ( o_change.oc_book_type, o_change.oc_level_side ) ; 
        rm_price_level = o_change.oc_level_num;
        rm_entry_size  = o_change.oc_new_qty;
        rm_entry_px    = level.price;
        rm_num_orders  = level.num_orders
    } in 
    { state with msg_queue = change_m :: state.msg_queue }
;;

(** 2.3 Order Delete internal event         *)
(**     - TODO What goes into level delete? *)
type ord_del_data = { 
    od_sec_type : sec_type;
    od_book_type : book_type;
    od_level_num : int;
    od_level_side : order_side;
};;

(** 2.3.1 Order Delete seding         *)
let send_o_del (state, o_del) = 
    let state = advance_rep_seq_num ( state, o_del.od_sec_type ) in
    let level = get_level (state , o_del.od_sec_type, o_del.od_book_type, o_del.od_level_side, o_del.od_level_num ) in
    match level with None -> state | Some level -> 
    let del_m = RefreshMessage {
        rm_security_id = get_security_id ( state, o_del.od_sec_type ) ;
        rm_rep_seq_num = get_rep_seq_num ( state, o_del.od_sec_type ) ;
        rm_msg_type    = V_MDUpdateAction_Delete ;

        rm_entry_type  = side_to_entry_type ( o_del.od_book_type, o_del.od_level_side ) ; 
        rm_price_level = o_del.od_level_num;
        rm_entry_size  = level.qty;
        rm_entry_px    = level.price;
        rm_num_orders  = level.num_orders
    } in 
    { state with msg_queue = del_m :: state.msg_queue }
;;



(** 2.4 Snapshot sending  *)

(** 2.4.1 This creates a message with a single level of a book*)
let make_snapshot_message (state, sec_type, level ) = 
    SnapshotMessage {
        sm_security_id                = get_security_id ( state, SecA );
        sm_last_msg_seq_num_processed = state.p_seq_num;
        sm_rep_seq_num                = get_rep_seq_num ( state, SecA );
        sm_real_bid = get_level ( state, sec_type, Book_Type_Multi,   OrdBuy,  level ); 
        sm_real_ask = get_level ( state, sec_type, Book_Type_Multi,   OrdSell, level ); 
        sm_imp_bid  = get_level ( state, sec_type, Book_Type_Implied, OrdBuy,  level );
        sm_imp_ask  = get_level ( state, sec_type, Book_Type_Implied, OrdSell, level ); 
    } 
;;

(** 2.4.1 This generates messages for each security and each level of its books *)
let send_snapshot (state) = 
    let snaps = state.msg_queue in
    let state = advance_rep_seq_num   ( state, SecA    )        in
    let snaps = make_snapshot_message ( state, SecA, 1 )::snaps in
    let state = advance_rep_seq_num   ( state, SecA    )        in
    let snaps = make_snapshot_message ( state, SecA, 2 )::snaps in
    let state = advance_rep_seq_num   ( state, SecA    )        in
    let snaps = make_snapshot_message ( state, SecA, 3 )::snaps in
    let state = advance_rep_seq_num   ( state, SecA    )        in
    let snaps = make_snapshot_message ( state, SecA, 4 )::snaps in
    let state = advance_rep_seq_num   ( state, SecA    )        in
    let snaps = make_snapshot_message ( state, SecA, 5 )::snaps in
    let state = advance_rep_seq_num   ( state, SecB    )        in
    let snaps = make_snapshot_message ( state, SecB, 1 )::snaps in
    let state = advance_rep_seq_num   ( state, SecB    )        in
    let snaps = make_snapshot_message ( state, SecB, 2 )::snaps in
    let state = advance_rep_seq_num   ( state, SecB    )        in
    let snaps = make_snapshot_message ( state, SecB, 3 )::snaps in
    let state = advance_rep_seq_num   ( state, SecB    )        in
    let snaps = make_snapshot_message ( state, SecB, 4 )::snaps in
    let state = advance_rep_seq_num   ( state, SecB    )        in
    let snaps = make_snapshot_message ( state, SecB, 5 )::snaps in
    { state with msg_queue = snaps; }
;;




(** Possible events at the exchange *)
type int_state_trans =
    | ST_BookReset                  (* We reset the whole book *)
    | ST_Add    of ord_add_data     (* Add order book level *)
    | ST_Change of ord_change_data  (* Cancel an order in the book *)
    | ST_Delete of ord_del_data     (* Delete book level *)
    | ST_DataSend                   (* Indicates that we need to send out the event *)
    | ST_Snapshot                   (* Indicates the need to send a snapshot message *)
;;

(** Check that the level exists for the whole security *)
let sec_level_exists (state, sec_t, book_t, order_s, level_n : exchange_state * sec_type * book_type * order_side * int ) = 
    match get_level  (state, sec_t, book_t, order_s, level_n ) with None -> false | Some _ -> true 
;;

(** Define a valid transition of the exchange *)
let is_trans_valid (state, trans) =
    match trans with
    | ST_BookReset -> true (** We can generally reset the book *)
    | ST_Add oa_data -> 
        not (sec_level_exists ( state, 
                                oa_data.oa_sec_type, 
                                oa_data.oa_book_type,
                                oa_data.oa_level_side,
                                oa_data.oa_level_num )) && 
        oa_data.oa_level_num > 0 &&
        oa_data.oa_level_num < 6

    | ST_Change oc_data ->
        sec_level_exists (  state, 
                            oc_data.oc_sec_type,
                            oc_data.oc_book_type,
                            oc_data.oc_level_side,
                            oc_data.oc_level_num) && 
        oc_data.oc_level_num > 0 && 
        oc_data.oc_level_num < 6 &&
        oc_data.oc_new_qty > 0 
    
    | ST_Delete od_data -> 
        sec_level_exists (  state,
                            od_data.od_sec_type,
                            od_data.od_book_type,
                            od_data.od_level_side,
                            od_data.od_level_num
                            )

    | ST_DataSend -> true
    | ST_Snapshot -> true
;;


(** Send the packet *)
let send_packet (state) = 
    let new_p = {
        packet_seq_num  = state.p_seq_num;
        packet_messages = state.msg_queue;
    } in 
    { state with 
        pac_queue = new_p :: state.pac_queue;
        msg_queue = [];
        p_seq_num = state.p_seq_num + 1;
    }
;;


(** Here we actually maintain the order book and its states *)
let process_int_trans (state, trans) =
    match trans with 
    | ST_BookReset -> state
    | ST_Add o_add ->
        send_add_level (state, o_add)
    | ST_Change o_change -> send_o_change (state, o_change)
    | ST_Delete o_del -> send_o_del (state, o_del)
    | ST_DataSend -> send_packet (state)
    | ST_Snapshot -> send_snapshot (state)
;;

let empty_book_side = {
    one = None;
    two = None;
    three = None;
    four = None;
    five = None;
};;


let init_state = {
    p_seq_num = 1;
    sec_a = {
        sec_id = 1;
        last_rep_seq_num = 1;
        multi_book = {
            buy_orders = empty_book_side;
            sell_orders = empty_book_side;
        };
        implied_book = {
            buy_orders = empty_book_side;
            sell_orders = empty_book_side;
        };
    };
    sec_b = {
        sec_id = 2;
        last_rep_seq_num = 1;
        multi_book = {
            buy_orders = empty_book_side;
            sell_orders = empty_book_side;
        };
        implied_book = {
            buy_orders = empty_book_side;
            sell_orders = empty_book_side;
        };
    };

    msg_queue = [];
    pac_queue = [];
};;

(** testgen *)
let t_three (msg1, msg2, msg3) = 
    let s = init_state in 
    let s1 = process_int_trans (s, msg1) in 
    let s2 = process_int_trans (s1, msg2) in 
    process_int_trans (s2, msg3)
;;

(** Are these transitions valid? *)
let vt (msg1, msg2, msg3) = 
    let s = init_state in 
    let s1 = process_int_trans (s, msg1) in 
    let s2 = process_int_trans (s1, msg2) in 
    let s3 = process_int_trans (s2, msg3) in 
    is_trans_valid (s, msg1) && 
    is_trans_valid (s1, msg2) && 
    is_trans_valid (s2, msg3)
;;
