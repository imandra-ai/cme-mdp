(**
    Aesthetic Integration Ltd.
    Copyright 2016

    CME_Exchange.ml
*)


(* @meta[imandra_ignore] on @end *)
open CME_Types;;
(* @meta[imandra_ignore] off @end *)

(** 1. Internal state *)

(** 1.1 Books. *)
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
    | 5 -> bs.five
    | _ ->  NoLevel
;;

(** 1.1.2 Book access function: set level  *)
let set_obook_level (bs, level_num, level : book_side * int * order_level ) = 
    match level_num with 
    | 1 -> { bs with one   = level } 
    | 2 -> { bs with two   = level }
    | 3 -> { bs with three = level }
    | 4 -> { bs with four  = level } 
    | 5 -> { bs with five  = level }
    | _ -> bs
;;

(** 1.1.4 Empty book constant *)
let empty_book_side =
    let emptyL =  NoLevel in
    { one = emptyL; two = emptyL; three = emptyL; four = emptyL; five = emptyL };;

(** 1.2 Security state including the order book.                    *)
type security_state = {
    last_rep_seq_num : int;     (* Last RepSeqNum for the sc        *)
    sec_id : int;               (* Security ID                      *)
    multi_book : order_book;    (* Multi depth book                 *)
    implied_book : order_book;  (* Implied book                     *)
};;

(** 1.3 The state of the whole exchange.                            *)
type exchange_state = {
    sec_a : security_state;     (* Security A orderbook             *)
    sec_b : security_state;	    (* Security B orderbook             *)

    (** Queue of events that have occured since the last packet submission *)
     inc_msg_queue : message list;
    snap_msg_queue : message list;

    (** Packets queue                                               *)
    pac_queue: packet list;
    last_inc_seq_num  : int;            (* Packet sequence number   *)
    last_snap_seq_num : int;            (* Packet sequence number   *)

    (** Testgen-specific state-space restrictions                   *)
    num_resets : int
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

type book_type = 
    | Book_Type_Implied 
    | Book_Type_Multi 
;;

(* entry_type utility function *)
let side_to_entry_type ( book_type, side : book_type * order_side) = 
    match ( book_type, side ) with 
    | ( Book_Type_Implied  , OrdBuy  ) -> V_MDEntryType_ImpliedBid
    | ( Book_Type_Multi    , OrdBuy  ) -> V_MDEntryType_Bid
    | ( Book_Type_Implied  , OrdSell ) -> V_MDEntryType_ImpliedOffer
    | ( Book_Type_Multi    , OrdSell ) -> V_MDEntryType_Offer
;;

let get_level ( state, security, book_type, order_side, nlevel  : exchange_state * sec_type * book_type * order_side * int ) =
    let s_state = match security with | SecA -> state.sec_a | SecB -> state.sec_b in
    match book_type, order_side with 
        | Book_Type_Multi    , OrdBuy  -> get_obook_level ( s_state.multi_book.buy_orders    , nlevel )
        | Book_Type_Implied  , OrdBuy  -> get_obook_level ( s_state.implied_book.buy_orders  , nlevel )
        | Book_Type_Multi    , OrdSell -> get_obook_level ( s_state.multi_book.sell_orders   , nlevel )
        | Book_Type_Implied  , OrdSell -> get_obook_level ( s_state.implied_book.sell_orders , nlevel )
;;

let set_level ( state, security, book_type, order_side, nlevel, level  : exchange_state * sec_type * book_type * order_side * int * order_level ) =
    let s_state = match security with | SecA -> state.sec_a | SecB -> state.sec_b in
    let s_state = match book_type, order_side with 
        | Book_Type_Multi    , OrdBuy  -> { s_state with multi_book   = { s_state.multi_book   with  buy_orders = set_obook_level ( s_state.multi_book.buy_orders    , nlevel , level ) } }
        | Book_Type_Implied  , OrdBuy  -> { s_state with implied_book = { s_state.implied_book with  buy_orders = set_obook_level ( s_state.implied_book.buy_orders  , nlevel , level ) } }
        | Book_Type_Multi    , OrdSell -> { s_state with multi_book   = { s_state.multi_book   with sell_orders = set_obook_level ( s_state.multi_book.sell_orders   , nlevel , level ) } }
        | Book_Type_Implied  , OrdSell -> { s_state with implied_book = { s_state.implied_book with sell_orders = set_obook_level ( s_state.implied_book.sell_orders , nlevel , level ) } }
        in
    match security with 
        | SecA -> { state with sec_a = s_state }
        | SecB -> { state with sec_b = s_state }
;;

let advance_rep_seq_num ( state, sec_type ) =
    match sec_type with 
    | SecA -> { state with sec_a = {state.sec_a with last_rep_seq_num = state.sec_a.last_rep_seq_num } }
    | SecB -> { state with sec_b = {state.sec_b with last_rep_seq_num = state.sec_b.last_rep_seq_num } }
;;

let reset_books_exchange state = 
    let empty = { buy_orders = empty_book_side; sell_orders = empty_book_side } in
    { state with 
        sec_a = {state.sec_a with multi_book = empty; implied_book = empty } ;
        sec_b = {state.sec_a with multi_book = empty; implied_book = empty } ;
        num_resets = state.num_resets + 1 }
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
    let side       = o_add.oa_level_side in
    let price      = o_add.oa_price      in
    let qty        = o_add.oa_order_qty  in
    let num_orders = o_add.oa_num_orders in
    let nlevel     = o_add.oa_level_num  in
    let add_m = RefreshMessage {
        rm_security_id = get_security_id ( state, o_add.oa_sec_type ) ;
        rm_rep_seq_num = get_rep_seq_num ( state, o_add.oa_sec_type ) ;
        rm_msg_type    = V_MDUpdateAction_New ;

        rm_entry_type  = side_to_entry_type ( o_add.oa_book_type, side ) ; 
        rm_price_level = nlevel;
        rm_entry_px    = price;
        rm_entry_size  = qty;
        rm_num_orders  = num_orders
    } in 
    let newlevel = Level { side; price; qty; num_orders } in
    let state = set_level ( state, o_add.oa_sec_type, o_add.oa_book_type, side, nlevel, newlevel ) in 
    { state with inc_msg_queue = add_m :: state.inc_msg_queue }
;;

(** 2.1.2 Check whether add keeps the sorted book *)
let add_respects_order ( state, oa_data ) =
    let stype      = oa_data.oa_sec_type   in
    let btype      = oa_data.oa_book_type  in
    let side       = oa_data.oa_level_side in
    let price      = oa_data.oa_price      in
    let qty        = oa_data.oa_order_qty  in
    let num_orders = oa_data.oa_num_orders in
    let prev_level = get_level ( state, stype, btype, side, oa_data.oa_level_num - 1 ) in
    let  new_level = Level { side; price; qty; num_orders } in
    let next_level = get_level ( state, stype, btype, side, oa_data.oa_level_num + 1 ) in
     order_higher_ranked (side, new_level, next_level) &&
    (order_higher_ranked (side, prev_level, new_level) || (oa_data.oa_level_num = 1) ) &&
    if oa_data.oa_level_num <> 1 then true else begin
        let top_buy  = if side = OrdBuy  then new_level else get_level ( state, stype, btype, OrdBuy,  1) in
        let top_sell = if side = OrdSell then new_level else get_level ( state, stype, btype, OrdSell, 1) in
        match (top_buy, top_sell) with 
        | NoLevel , _       -> true
        |       _ , NoLevel -> true
        | Level b , Level s -> b.price <= s.price
    end
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
    let state  = advance_rep_seq_num ( state, o_change.oc_sec_type ) in
    let side   = o_change.oc_level_side in
    let nlevel = o_change.oc_level_num  in
    let level  = get_level (state , o_change.oc_sec_type, o_change.oc_book_type, side, nlevel) in
    match level with NoLevel -> state | Level level -> 
    let level = { level with qty =  o_change.oc_new_qty } in
    let change_m = RefreshMessage {
        rm_security_id = get_security_id ( state, o_change.oc_sec_type ) ;
        rm_rep_seq_num = get_rep_seq_num ( state, o_change.oc_sec_type ) ;
        rm_msg_type    = V_MDUpdateAction_Change ;

        rm_entry_type  = side_to_entry_type ( o_change.oc_book_type, side ) ; 
        rm_price_level = nlevel;
        rm_entry_size  = level.qty;
        rm_entry_px    = level.price;
        rm_num_orders  = level.num_orders
    } in 
    let state = set_level ( state, o_change.oc_sec_type, o_change.oc_book_type, side, nlevel, Level level ) in 
    { state with inc_msg_queue = change_m :: state.inc_msg_queue }
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
    let side   = o_del.od_level_side in
    let nlevel = o_del.od_level_num  in
    let level = get_level (state , o_del.od_sec_type, o_del.od_book_type, side, nlevel ) in
    match level with NoLevel -> state | Level level -> 
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
    let state = set_level ( state, o_del.od_sec_type, o_del.od_book_type, side, nlevel, NoLevel ) in 
    { state with inc_msg_queue = del_m :: state.inc_msg_queue }
;;



(** 2.4 Snapshot sending  *)

(** 2.4.1 This creates a list of orders on a given side of a book *)
let get_level_list (state, sec_type, book_type, side ) = [ 
    get_level ( state, sec_type, book_type,  side, 1 ) ; 
    get_level ( state, sec_type, book_type,  side, 2 ) ; 
    get_level ( state, sec_type, book_type,  side, 3 ) ; 
    get_level ( state, sec_type, book_type,  side, 4 ) ; 
    get_level ( state, sec_type, book_type,  side, 5 ) ; 
];;

(** 2.4.2 This generates a snapshot message for the whole book of one security *)
let send_snapshot ( state, sec_type ) = 
    let state = advance_rep_seq_num  ( state, sec_type )        in
    let m_snap  = SnapshotMessage {
        sm_security_id = get_security_id ( state, sec_type ) ;
        sm_rep_seq_num = get_rep_seq_num ( state, sec_type ) ;
        sm_snapshot = {
            snap_m_book = {
                buys  = get_level_list ( state, sec_type, Book_Type_Multi, OrdBuy  );
                sells = get_level_list ( state, sec_type, Book_Type_Multi, OrdSell ); 
            };
            snap_i_book = {
                buys  = get_level_list ( state, sec_type, Book_Type_Implied, OrdBuy  );
                sells = get_level_list ( state, sec_type, Book_Type_Implied, OrdSell ); 
            }; 
            snap_last_msg_seq_num_processed = state.last_inc_seq_num;
        };
    } in     
    { state with snap_msg_queue = m_snap::state.snap_msg_queue; }
;;

(** 3. Possible events at the exchange *)
type int_state_trans =
    | ST_BookReset                    (* We reset the whole book *)
    | ST_Add      of ord_add_data     (* Add order book level *)
    | ST_Change   of ord_change_data  (* Cancel an order in the book *)
    | ST_Delete   of ord_del_data     (* Delete book level *)
    | ST_DataSendInc                  (* Indicates that we need to send out the collected incremental refresh messages *)
    | ST_DataSendSnap                 (* Indicates that we need to send out the collected snapshot refresh messages *)
    | ST_Snapshot of sec_type         (* Indicates the need to send a snapshot message *)
;;

(** 3.1 Check that the level exists for the whole security *)
let sec_level_exists (state, sec_t, book_t, order_s, level_n : exchange_state * sec_type * book_type * order_side * int ) = 
    match get_level  (state, sec_t, book_t, order_s, level_n ) with NoLevel -> false | Level _ -> true 
;;

(** 3.2 Define a valid transition of the exchange *)
let is_trans_valid (state, trans) =
    match trans with
      | ST_BookReset ->
        (* For testgen, let's limit the number of resets to 2.
           Note we start counting at 0. *)
        state.num_resets < 2
      | ST_Add oa_data ->
        not (sec_level_exists ( state, 
                                oa_data.oa_sec_type, 
                                oa_data.oa_book_type,
                                oa_data.oa_level_side,
                                oa_data.oa_level_num )) && 
        oa_data.oa_level_num > 0 &&
        oa_data.oa_level_num < 6 &&
        oa_data.oa_order_qty > 0 &&
        add_respects_order ( state, oa_data )

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
    | ST_DataSendInc  -> state.inc_msg_queue  <> []
    | ST_DataSendSnap -> state.snap_msg_queue <> []
    | ST_Snapshot _ -> true
;;


(*  4. Packet sending *)

(** 4.1 Send snapshot packet *)
let send_snap_packet (state) = 
    let new_p = {
        packet_seq_num  = state.last_snap_seq_num + 1;
        packet_messages = state.snap_msg_queue;
        packet_channel  = Ch_Snap_A
    } in 
    { state with 
        pac_queue = new_p :: { new_p with packet_channel = Ch_Snap_B } :: state.pac_queue;
        snap_msg_queue = [];
        last_snap_seq_num = state.last_snap_seq_num + 1;
    }
;;

(** 4.2 Send incremental refresh packet *)
let send_inc_packet (state) = 
    let new_p = {
        packet_seq_num  = state.last_inc_seq_num + 1;
        packet_messages = state.inc_msg_queue;
        packet_channel  = Ch_Ref_A
    } in 
    { state with 
        pac_queue = new_p :: { new_p with packet_channel = Ch_Ref_B } :: state.pac_queue;
        inc_msg_queue = [];
        last_inc_seq_num = state.last_inc_seq_num + 1;
    }
;;

(** Here we actually maintain the order book and its states *)
let process_int_trans (state, trans) =
    match trans with 
    | ST_BookReset         -> reset_books_exchange state
    | ST_Add o_add         -> send_add_level (state, o_add)
    | ST_Change o_change   -> send_o_change (state, o_change)
    | ST_Delete o_del      -> send_o_del (state, o_del)
    | ST_DataSendInc       -> send_inc_packet  (state)
    | ST_DataSendSnap      -> send_snap_packet (state)
    | ST_Snapshot sec_type -> send_snapshot (state, sec_type)
;;

let init_ex_state = {
    sec_a = {
        sec_id = 1;
        last_rep_seq_num = 0;
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
        last_rep_seq_num = 0;
        multi_book = {
            buy_orders = empty_book_side;
            sell_orders = empty_book_side;
        };
        implied_book = {
            buy_orders = empty_book_side;
            sell_orders = empty_book_side;
        };
    };
    
    last_snap_seq_num = 0;
    last_inc_seq_num   = 0;
    inc_msg_queue = [];
    snap_msg_queue = [];
    pac_queue = [];
    num_resets = 0
};;

(** simulate *)
let rec simulate_exchange ( s, int_tran_list : exchange_state * int_state_trans list ) = 
    match int_tran_list with 
    | [] -> s
    | x::xs -> simulate_exchange (process_int_trans (s, x), xs)
;;

