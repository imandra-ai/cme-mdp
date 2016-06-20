(**
    Aesthetic Integration Ltd.
    Copyright 2016
    CME.ml
*)

let dec_units = 1000;;

(** Helper functions to convert between various *)
let dec_to_float (d) =
    (float_of_int (d / dec_units)) +. ((float_of_int (d mod dec_units)) /. (float_of_int dec_units));;

let float_to_dec (f) =
    int_of_float (f *. float_of_int(dec_units));;

type side = BUY | SELL;;
type opt_int_type = int option;;

(** Order_info is used to represent levels within order book *)
type order_info = {
    side : side;
    qty : int;
    price : int;
    num_orders : opt_int_type; (* Not provided for implied books *)
};;

(** We will use this type for setting property of our security *)
type liq_type = Liquid | Illiquid | LiqUnknown;;

(** Order_level *)
type order_level = NoLevel | Level of order_info;;

(** *************************************************************** *)
(** Define the types that we support                                *)
(** *************************************************************** *)
type sec_type =
    | FUTURES
    | SPREAD
    | OPTION
;;

type book_type =
    | Book_Type_Implied
    | Book_Type_Multi
    | Book_Type_Combined
;;
(** *************************************************************** *)

(** The various types *)
type msg_type =
    | V_MDUpdateAction_New
    | V_MDUpdateAction_Change
    | V_MDUpdateAction_Delete
    | V_MDUpdateAction_DeleteThru
    | V_MDUpdateAction_DeleteFrom
    | V_MDUpdateAction_Overlay
;;

(** *************************************************************** *)
(* Indicates which side is aggressor of the trade. If there is a    *)
(* zero value present, then there is no aggressor.                  *)
(** *************************************************************** *)
type aggressor_side =
    NoAggressor
    | BuyAggressor
    | SellAggressor
;;

(** *************************************************************** *)
(* Entry type                                                       *)
(** *************************************************************** *)
type entry_type =
    V_MDEntryType_Bid
  | V_MDEntryType_Offer
  | V_MDEntryType_ImpliedBid
  | V_MDEntryType_ImpliedOffer
  | V_MDEntryType_TradeSummary
  | V_MDEntryType_OpeningPrice
  | V_MDEntryType_SettlementPrice
  | V_MDEntryType_TradingSessionHighPrice
  | V_MDEntryType_TradingSessionLowPrice
  | V_MDEntryType_SessionHighBid
  | V_MDEntryType_SessionLowOffer
  | V_MDEntryType_TradeVolume
  | V_MDEntryType_OpenInterest
  | V_MDEntryType_FixingPrice
  | V_MDEntryType_EmptyBook
  | V_MDEntryType_ElectronicVolume
  | V_MDEntryType_ThresholdLimits
;;
(** *************************************************************** *)

(** *************************************************************** *)
(** Msg: TODO Need to replace this with actual packets              *)
(** *************************************************************** *)
type ref_message = {
    rm_security_id : int;
    rm_rep_seq_num : int;
    rm_msg_type : msg_type;

    rm_entry_type : entry_type; (* Bid, etc. *)
    rm_price_level : int;
    rm_entry_size : int;
    rm_entry_px : int;
    rm_num_orders : int;
}
;;

(** Internal snapshot message representation.                           *)
(*  Note that in 'real' format, this would be spread across numerous    *)
(*  messages within the same (potentially different) packets.           *)
type snap_message = {
    sm_security_id : int;
    sm_last_msg_seq_num_processed : int;    (* this corresponds to packet number for Incremental update *)
    sm_rep_seq_num : int;               (* this corresponds to instrument RepSeqNum *)

    sm_real_bid : order_level;
    sm_real_ask : order_level;
    sm_imp_bid : order_level;
    sm_imp_ask : order_level;
}
;;
(** *************************************************************** *)

(** *************************************************************** *)
(* This is the top-level packet type including the global sequence  *)
(* number                                                           *)
(** *************************************************************** *)

(** Note about connecting Snapshots and Incremental Refresh messages:
        --> Market recovery packet message contains field 369-LastMsgSeqNumProcessed
            it corresponds to Incremental Refresh Message
        --> Tag 83 RptSeq
*)
type packet_header = {
    ph_packet_seq_num : int;
    (** Corresponds to MsgSeqNum
        Packet sequence number.
        A unique sequence number given to each packet sent.
        Each channel will have its own separate set of sequence numbers that will increment sequentially
        with each packet and reset weekly.
        See: http://www.cmegroup.com/confluence/display/EPICSANDBOX/MDP+3.0+-+Binary+Packet+Header
     *)
    ph_sending_time : int;
    (** Corresponds to SendingTime
        UTC Time of message transmission by the Gateway. UTC Timestamps are sent in number of nanoseconds
        since Unix epoch with guaranteed microsecond precision.
    *)
};;


(** Note about packet data types:
    In the actual binary (and other) format, a packet has a header and a *list* of messages. For the purposes
    of modelling the feed, we do not need to introduce this complexity. In the model, we use ref_packet and
    snap_packet types. Note the at we have no restriction on repeating packet sequence numbers, hence a natural
    way to replicate a *real* packet with multiple messages is just by having a list of *model* packets with
    the same packet header, but different message fields.
    Types ref_packet_lst & snap_packet_lst are to be used by the encoder/decoder to read/write from/into
    the binary format.
    TODO: write utility functions to convert between ref_packet/ref_packet_lst types and similarly for
    snap_packet.
*)


type ref_packet = {
    rp_header : packet_header;
    rp_msg : ref_message;
};;

type snap_packet = {
    sp_header : packet_header;
    sp_snap : snap_message;
};;

(** The following two types of messages lists are directly used by the binary encoder/decoder *)
type ref_packet_lst = {
    rpl_header : packet_header;
    rpl_msgs : ref_message list;
};;

type snap_packet_lst = {
   spl_header : packet_header;
   spl_snap : snap_message list;
};;

(** We have two types of packets here: for Market Recovery (Snapshot) and Incremental
    Refresh *)
type packet = SnapshotPacket of snap_packet | IncRefreshPacket of ref_packet | NoPacket;;

(** These types represent the internal change book-keeping *)
type book_change_type =
    | Book_Changed_to_InRecovery    (* Changed status from Normal to InRecovery *)
    | Book_Changed_to_Normal        (* Changed stauts from InRecovery to Normal *)
    | Book_Proc_Normal_Update       (* Processed a packet in Normal mode, stayed in Normal *)
    | Book_Proc_Cache_Add           (* Processed a packet in InRecovery mode, stayed in InRecovery *)
;;

(** Generic book type (used for all three types of books) *)
type book = {
    buys : order_level list;
    sells : order_level list;
};;

type channel_type =
  | Ch_Ref_A
  | Ch_Ref_B
  | Ch_Snap_A
  | Ch_Snap_B
;;

(** This represents the current state of the books *)
type book_status =
  | Empty
  | Publishable
;;

(** Contains the various books that are held etc... *)
type books = {
    book_depth : int;
    multi : book;
    implied : book;
    combined : book;
    b_status : book_status;
};;

(** We only have two statuses: either we're in Normal or InRecovery *)
type feed_status =
    | InRecovery
    | Normal
;;

(** Create a list of empty list of order levels *)
let rec empty_order_levels (x) =
    if x > 0 then NoLevel :: empty_order_levels (x - 1) else []
;;

(** Create the empty book for a given depth *)
let empty_book (num_levels : int) =
{
    buys = empty_order_levels (num_levels);
    sells = empty_order_levels (num_levels);
}
;;

(** States of the refresh and snapshot channels *)
type ref_channel_state =
{
    r_unproc_packets : ref_packet list;  (** Unprocessed packets *)
    r_proc_packets : ref_packet list;    (** Processed packets *)
};;

type snap_channel_state = 
{
    s_unproc_packets : snap_packet list;
    s_proc_packets : snap_packet list; 
};;

(** This is how we will store the snapshot message *)
type snapshot = {
    snap_m_book : book;
    snap_i_book : book;
    snap_seq_num : int;
};;

(** ******************************************************* *)
(** This structure represents the history that we maintain  *)
(** ******************************************************* *)
type cycle_hist = {
    reference_sec_id : int;             (** Reference security *)
    self_sec_id : int;                  (** This security ID *)
    ref_sec_snap_received : bool;       (**  *)
    liq : liq_type;
}
;;

(** Structure with clean history *)
let clean_cycle_hist = {
   reference_sec_id = -1;
   self_sec_id = -1;
   ref_sec_snap_received = false;
   liq = LiqUnknown;
}
;;

(** We're encapsulating all of the messaging queues here *)
type channels = {
    ref_a : ref_channel_state;      (* Incremental Refresh Channel A *)
    ref_b : ref_channel_state;      (* Incremental Refresh Channel B *)

    last_seq_processed : int;       (* Last correctly processed sequence number *)
    cache : ref_message list;       (* These are cached incremental packets *)

    (** Last available snapshot -> updated only during recovery mode *)
    last_snapshot : snapshot option;

    (** Refresh channels that we subscribe to only when in recovery mode *)
    snap_a : snap_channel_state;    (* Full Refresh Channel A *)
    cycle_hist_a : cycle_hist;      (* Cycle history for channel A *)

    snap_b : snap_channel_state;    (* Full Refresh Channel B *)
    cycle_hist_b : cycle_hist;      (* Cycle history for channel B *)
}
;;

(**
    What are the state transitions:
    --> Invalid to Valid --> Sequence numbers have been recovered -> no need to listen to snapshot
    --> Invalid to Valid --> Snapshot received and all sequence numbers since the snapshot
    --> Reset packet received --> Snapshot along with all sequence numbers since that period
*)

(** Used for recording changes to the state (will be used *)
(** for test-suite generation *)
type internal_msg =
{
    im_change_type : book_change_type;
    im_time : int;
    im_books : books;
    im_cache : ref_message list;    
};;

(** Contains full definition of the state of the exchange *)
type feed_state = {

    (* Keeping a record of the security that we're maintaining *)
    sec_type : sec_type;
    sec_id   : int;

    (* The books that we're maintaining, etc... *)
    books : books;

    (* Communication channels *)
    channels : channels;

    feed_status : feed_status;

    (** This is a queue of internal packets that have been generated *)
    internal_changes : internal_msg list;

    (** This is the internal sequence number *)
    cur_time : int;
};;

(*  Check whether the cache is complete: that we have all of the packets
    in the sequence starting with the last processed *)
let rec check_list (msgs, last_idx : ref_message list * int) =
    match msgs with
    | [] -> true
    | x::xs -> if x.rm_rep_seq_num = last_idx + 1 then check_list (xs, last_idx + 1) else false
;;

(* Check to see whether the cache we're maintaining is correct since the valid sequence number *)
let is_cache_valid_since_seq_num (cache, last_processed_seq : ref_message list * int) =
    cache <> [] && check_list (cache, last_processed_seq)
;;

(** ************************************************************** *)
(** Book sorting information                                       *)
(** ************************************************************** *)
let order_higher_ranked (s, o1, o2 : side * order_level * order_level) =
    match o1, o2 with
    | Level d_1, Level d_2 -> if s = BUY then d_1.price >= d_2.price else d_1.price <= d_2.price
    | Level d_1, NoLevel -> true
    | NoLevel, Level d_2 -> false
    | NoLevel, NoLevel -> true
;;

(** Insert order into the book *)
let rec insert_order (a, s, orders : order_level * side * order_level list) =
  match orders with
  | [] -> [a]
  | x::xs ->
     if order_higher_ranked(s, a, x) then
       a::x::xs else x::insert_order(a, s, xs)
;;

(** Sort a single side of the book *)
let rec sort_side (orders, side : order_level list * side) =
    match orders with
    | [] -> []
    | x::xs -> insert_order (x, side, sort_side (xs, side))
;;

(** Top-level sorting function *)
let sort_book (b : book) = {
    buys = sort_side (b.buys, BUY);
    sells = sort_side (b.sells, SELL);
};;

(** Remove all orders over num-levels *)
let rec trim_side ( ords, num_levels, curr_level : order_level list * int * int) =
    if curr_level <= num_levels then (
        match ords with
        [] -> []
       | x::xs -> x :: trim_side (xs, num_levels, curr_level + 1)
    )
    else
        []
;;

(** Clean multi-depth book *)
let clean_multi_depth_book (books : books) =
    let book' = sort_book (books.multi) in
    let buys' = trim_side (book'.buys, books.book_depth, 1) in
    let sells' = trim_side (book'.sells, books.book_depth, 1) in
    { books with multi = book';
                 implied = sort_book (books.implied);
                 combined = {buys = buys'; sells = sells'} }
;;

(** Remove duplicates from the list of orders (used in conslidating the book) *)
let rec add_levels (orders : order_level list) =
    match orders with
    | [] -> []
    | [x] -> [x]
    | x :: y :: xs -> (
        match x, y with
        | Level x_d, Level y_d ->
            if x_d.price = y_d.price then
                let o = Level {
                    side = x_d.side;
                    qty = x_d.qty + y_d.qty;
                    price = x_d.price;
                    num_orders = None;
                } in ( o :: add_levels (xs))
            else
                x :: add_levels (y :: xs)
        | _, _ -> x :: add_levels (y :: xs)
        )
;;

(** Process a single channel msg *)
let process_snap_ch ( ch : snap_channel_state ) =
{
    s_proc_packets = List.hd ch.s_unproc_packets :: ch.s_proc_packets;
    s_unproc_packets = List.tl ch.s_unproc_packets;
};;

let process_ref_ch ( ch : ref_channel_state ) =
{
    r_proc_packets = List.hd ch.r_unproc_packets :: ch.r_proc_packets;
    r_unproc_packets = List.tl ch.r_unproc_packets;
};;


(** *************************************************************** *)
(** Functions used for book modifications                           *)
(** *************************************************************** *)
let rec adjust_orders_size (num_levels, ords : int * order_level list) =
    if num_levels <= 0 then
        ords
    else
        adjust_orders_size (num_levels - 1, ords @ [NoLevel])
;;

(** Adjusts the number of levels within a single side of an order book *)
let adjust_size (orders, book_depth : order_level list * int) =
    adjust_orders_size ((List.length orders - book_depth), orders)
;;

(** Used to remove orders from a list given starting index *)
let rec bk_delete_from (orders, curr_idx, target_idx : order_level list * int * int ) =
    match orders with
        | [] -> []
        | x::xs ->
            if curr_idx = target_idx then
                []
            else
                x :: bk_delete_from (xs, curr_idx + 1, target_idx)
;;

(**
    Overlay a new order info
    TODO: Figure out what this is. *)
let bk_overlay (orders, id : order_info list * int) =
    []
;;

(** Delete a level from the list of orders information *)
let rec delete_level (orders, curr_idx, target_idx : order_level list * int * int) =
    match orders with
        | [] -> []
        | x::xs ->
            if curr_idx = target_idx then
                xs
            else
                x :: delete_level (xs, curr_idx + 1, target_idx)
;;

(** Delete from the book *)
let bk_delete (orders, price_level : order_level list * int) =
    delete_level (orders, 1, price_level)
;;

(** Change existing order level *)
let rec ch_inplace (orders, curr_idx, target_idx, new_order_info : order_level list * int * int * order_level) =
    match orders with
        | [] -> if curr_idx = target_idx then [new_order_info] else []
        | x::xs ->
            if curr_idx = target_idx then
                new_order_info :: xs
            else
                x :: ch_inplace (xs, curr_idx + 1, target_idx, new_order_info)
;;

(** Change an existing order level *)
let bk_change (orders, side, price_level, entry_size, entry_price, num_orders : order_level list * side * int * int * int * opt_int_type) =
    let new_order_info = Level {
        side = side;
        qty = entry_size;
        price = entry_price;
        num_orders = num_orders;
    } in ch_inplace (orders, 1, price_level, new_order_info)
;;

(** Helper function for 'bk_new' *)
let rec insert_level ( orders, new_order, curr_idx, target_idx : order_level list * order_level * int * int) =
    match orders with
        | [] -> [ new_order ]
        | x::xs ->
            if curr_idx = target_idx then
                new_order :: xs
            else
                x :: insert_level (xs, new_order, curr_idx + 1, target_idx)
;;

(** Add a new level to the book *)
let bk_new (orders, side, price_level, entry_size, entry_price, num_orders : order_level list * side * int * int * int * opt_int_type) =
    let new_order_info = Level {
        side = side;
        qty = entry_size;
        price = entry_price;
        num_orders = num_orders;
    } in
    insert_level (orders, new_order_info, 1, price_level)
;;

let mk_int_msg (s, ct : feed_state * book_change_type) =
{
    im_cache = s.channels.cache;
    im_change_type = ct; (* We're transitionaing to recovery state*)
    im_time = s.channels.last_seq_processed;
    im_books = s.books;
};;

let get_cycle_hist (ch_a, ch_b, src) =
    if src = Ch_Ref_A then ch_a else ch_b
;;

(** We have a refresh cycle history for each of the refresh channels;
    Hence, we look at both of them to determine whether a security is
    liquid. *)
let is_security_liquid (ch : channels)=
    ch.cycle_hist_a.liq = Liquid || ch.cycle_hist_b.liq = Liquid
;;

(** *************************************************************** *)
(** Update cycle history for this channel                           *)
(** *************************************************************** *)
(**
    Is the liquidity state 'sticky' in that once a security is deemed
    illiquid, could it be reset?
*)
let update_cycle_hist (ch, snap : cycle_hist * snap_message) =
    let is_ref_sec = ch.reference_sec_id = snap.sm_security_id in
    let is_this_sec = ch.self_sec_id = snap.sm_security_id in

    let first_ref_sec = not (ch.ref_sec_snap_received) && is_ref_sec in
    let second_ref_sec = ch.ref_sec_snap_received && is_ref_sec in

    let liq' = if ch.liq = LiqUnknown then (
        if second_ref_sec then Illiquid
            else if is_this_sec then Liquid
            else ch.liq
        ) else ch.liq in {
        ch with
            ref_sec_snap_received = first_ref_sec;
            liq = liq';
    }
;;

(** We reset the cycle history if we transition out of Recovery state *)
let reset_cycle_hist ( ch : cycle_hist ) =
{
    clean_cycle_hist with reference_sec_id = ch.reference_sec_id
}
;;

(** *************************************************************** *)
(** Here we need to figure out what we're doing, etc.               *)
(** *************************************************************** *)
(** Security sequence number of the last message                    *)
let process_refresh_action (books, packet : books * snap_packet) =
    let snap = packet.sp_snap in
    let seq_num = snap.sm_last_msg_seq_num_processed in
    let depth = books.book_depth in
    let books' = {
        books with
            multi = empty_book (books.book_depth);
            implied = empty_book (books.book_depth);
            combined = empty_book (books.book_depth); } in
    let m = books'.multi in
    let m' = {
        buys = trim_side ( insert_order (snap.sm_real_bid, BUY, m.buys), depth, 1);
        sells = trim_side (insert_order (snap.sm_real_ask, SELL, m.sells), depth, 1);
    } in
    let i = books'.implied in
    let i' = {
        buys = trim_side ( insert_order (snap.sm_imp_bid, BUY, i.buys), depth, 1);
        sells = trim_side (insert_order (snap.sm_imp_ask, SELL, i.sells), depth, 1);
    } in {
       (** We're returning a snapshot message *)
        snap_m_book = m';
        snap_i_book = i';
        snap_seq_num = seq_num;
    }
;;

(** Replace the book information with snapshot data *)
let apply_snapshot (books, snap : books * snapshot) =
    {
        books with
            multi = snap.snap_m_book;
            implied = snap.snap_i_book;
            combined = empty_book (books.book_depth);
    }
;;

(** Is the latest snapshot better (later) than the existing one? *)
let new_snapshot_better (new_snap, old_snap : snapshot * snapshot option) =
    match old_snap with
        | None -> true
        | Some s ->
            new_snap.snap_seq_num <= s.snap_seq_num
;;

(** Change status of the books *)
let reset_books ( b : books ) =
{
    b with
    multi = empty_book (b.book_depth);
    implied = empty_book (b.book_depth);
    combined = empty_book (b.book_depth);
    b_status = Empty;
};;

(* *************************************************************** *)
(*  Note that here we're assuming that the message is in sequence, *)
(*  hence we will not check sequence numbers here                  *)
(* *************************************************************** *)
let process_md_update_action (books, msg : books * ref_message) =
    let m = books.multi in
    let i = books.implied in

    if msg.rm_entry_type = V_MDEntryType_EmptyBook then
        reset_books (books)

    else match msg.rm_msg_type with
    | V_MDUpdateAction_New -> (
        match msg.rm_entry_type with
        | V_MDEntryType_Bid ->
            let buys' = bk_new (m.buys, BUY, msg.rm_price_level, msg.rm_entry_size,
                                msg.rm_entry_px, Some msg.rm_num_orders) in
            let books' = { books with multi = { m with buys = buys'; }} in
            clean_multi_depth_book (books')
        | V_MDEntryType_Offer ->
            let sells' = bk_new (m.sells, SELL, msg.rm_price_level, msg.rm_entry_size,
                                 msg.rm_entry_px, Some msg.rm_num_orders) in
            let books' = { books with multi = { m with sells = sells'; }} in
            clean_multi_depth_book (books')
        | V_MDEntryType_ImpliedBid ->
            let buys' = bk_new (i.buys, BUY, msg.rm_price_level, msg.rm_entry_size,
                                msg.rm_entry_px, None) in
            let books' = { books with implied = {i with buys = buys'; }} in
            clean_multi_depth_book (books')
        | V_MDEntryType_ImpliedOffer ->
            let sells' = bk_new (i.sells, SELL, msg.rm_price_level, msg.rm_entry_size,
                                 msg.rm_entry_px, None) in
            let books' = { books with implied = {i with sells = sells'; }} in
            clean_multi_depth_book (books')
        | _ -> books
    )

    | V_MDUpdateAction_Change -> (
        match msg.rm_entry_type with
        | V_MDEntryType_Bid ->
           let buys' = bk_change (m.buys, BUY, msg.rm_price_level, msg.rm_entry_size,
                                  msg.rm_entry_px, Some msg.rm_num_orders)
           in { books with multi = sort_book { m with buys = buys' }}
        | V_MDEntryType_Offer ->
           let sells' = bk_change (m.sells, SELL, msg.rm_price_level, msg.rm_entry_size,
                                   msg.rm_entry_px, Some msg.rm_num_orders)
           in { books with multi = sort_book { m with sells = sells' }}
        | V_MDEntryType_ImpliedBid ->
           let buys' = bk_change (i.buys, BUY, msg.rm_price_level, msg.rm_entry_size,
                                  msg.rm_entry_px, None)
           in { books with implied = sort_book { i with buys = buys' }}
        | V_MDEntryType_ImpliedOffer ->
           let sells' = bk_change (i.sells, SELL, msg.rm_price_level, msg.rm_entry_size,
                                   msg.rm_entry_px, None)
           in { books with implied = sort_book { i with sells = sells' }}
        | _ -> books
    )

    | V_MDUpdateAction_Delete -> (
        (** Here we only care about the level that we're deleting, also need to make *)
        (** sure the size is filled in correctly with NoLevel objects after the delete is done *)
        match msg.rm_entry_type with
        | V_MDEntryType_Bid ->
            let buys' = bk_delete (m.buys, msg.rm_price_level)
            in { books with multi = sort_book { m with buys = adjust_size (buys', books.book_depth) }}
        | V_MDEntryType_Offer ->
            let sells' = bk_delete (m.sells, msg.rm_price_level)
            in { books with multi = sort_book { m with sells = adjust_size( sells', books.book_depth) }}
        | V_MDEntryType_ImpliedBid ->
            let buys' = bk_delete (i.buys, msg.rm_price_level)
            in { books with implied = sort_book { i with buys = adjust_size (buys', books.book_depth) }}
        | V_MDEntryType_ImpliedOffer ->
            let sells' = bk_delete (i.sells, msg.rm_price_level)
            in { books with implied = sort_book { i with sells = adjust_size( sells', books.book_depth) }}
        | _ -> books
    )

    | V_MDUpdateAction_DeleteThru -> (
        match msg.rm_entry_type with
        (** We're getting rid of all of the orders on one side here *)
        | V_MDEntryType_Bid -> { books with multi = sort_book { m with buys = []; }}
        | V_MDEntryType_Offer -> { books with multi = sort_book { m with sells = []; }}
        | V_MDEntryType_ImpliedBid -> { books with implied = sort_book { i with buys = []; }}
        | V_MDEntryType_ImpliedOffer -> { books with implied = sort_book { i with sells = []; }}
        | _ -> books
    )

    | V_MDUpdateAction_DeleteFrom -> (
        (** Delete all orders from a side from a given level *)
        match msg.rm_entry_type with
        | V_MDEntryType_Bid ->
            let buys' = bk_delete_from (m.buys, 1, msg.rm_price_level) in
            { books with multi = sort_book { m with buys = adjust_size (buys', books.book_depth); }}
        | V_MDEntryType_Offer ->
            let sells' = bk_delete_from (m.sells, 1, msg.rm_price_level) in
            { books with multi = sort_book { m with sells = adjust_size (sells', books.book_depth); }}
        | V_MDEntryType_ImpliedBid ->
            let buys' = bk_delete_from (i.buys, 1, msg.rm_price_level) in
            { books with implied = sort_book { i with buys = adjust_size (buys', books.book_depth); }}
        | V_MDEntryType_ImpliedOffer ->
            let sells' = bk_delete_from (i.sells, 1, msg.rm_price_level) in
            { books with implied = sort_book { i with sells = adjust_size (sells', books.book_depth); }}
        | _ -> books
    )

    | _ -> books (** We only care about the update actions here *)
;;
(** *************************************************************** *)

(** apply_cache *)
let rec apply_update_packets (books, packets : books * ref_message list) =
    match packets with
    | [] -> books
    | x::xs -> let books' = process_md_update_action (books, x)
                in apply_update_packets (books', xs)
;;

let apply_cache (books, channels : books * channels) =
    apply_update_packets (books, channels.cache)
;;

(**
    Check whether the feed can now fully recover.
    There are two cases:
    -- The cache is now valid -> it has the full sequence of packets since the
        last processed one
    -- We have a snapshot and a full sequence of packets since the snapshot.
    Alternatively, we have an illiquid security, hence we need to use the
    incremental refresh channels only. *)
let attempt_recovery (s : feed_state) =
    let books = s.books in
    let channels = s.channels in
    if channels.cycle_hist_a.liq = Illiquid && channels.cycle_hist_b.liq = Illiquid then
        let books' = apply_cache (books, channels) in
        { s with feed_status = Normal;
            books = books'; }
    else (
        if is_cache_valid_since_seq_num (channels.cache, channels.last_seq_processed) then (
            let books' = apply_cache (books, channels) in
            { s with feed_status = Normal;
                books = books'; }
        )
        else match channels.last_snapshot with
            | None -> s
            | Some snap ->
                if is_cache_valid_since_seq_num (channels.cache, snap.snap_seq_num) then
                    let books' = apply_snapshot (books, snap) in
                    let books'' = apply_cache (books', channels) in
                    { s with books = books''; feed_status = Normal; }
                else
                    s
        )
;;

(** Make all unprocessed packets to be processed *)
let clean_snap_ch (ch : snap_channel_state) =
{
    s_proc_packets = ch.s_unproc_packets;
    s_unproc_packets = [];
}
;;

let clean_ref_ch (ch : ref_channel_state) =
{
    r_proc_packets = ch.r_unproc_packets;
    r_unproc_packets = [];
}
;;


(** check to make sure we're processing the next one *)
let correct_seq (channels, msg : channels * ref_message) =
    msg.rm_rep_seq_num <> (channels.last_seq_processed + 1)
;;

(** Data structure used in returning the right packets *)
type msg_source =
{
    p : packet;
    source : channel_type;
}
;;

(** Select the right channel from the list *)
let get_snap_channel (channels, ct : channels * channel_type) =
    match ct with
    | Ch_Snap_A -> channels.snap_a
    | _         -> channels.snap_b

;;

let get_ref_channel (channels, ct : channels * channel_type) =
    match ct with
    | Ch_Ref_A  -> channels.ref_a
    | _         -> channels.ref_b
;;


(** set_channel_snap *)
let set_snap_channel (chs, ch, ch_t : channels * snap_channel_state * channel_type) =
{
    chs with
        snap_a = if ch_t = Ch_Snap_A then ch else chs.snap_a;
        snap_b = if ch_t = Ch_Snap_B then ch else chs.snap_b;
};;

(** set_channel_inc *)
let set_ref_channel (chs, ch, ch_t : channels * ref_channel_state * channel_type) =
{
    chs with
        ref_a = if ch_t = Ch_Ref_A then ch else chs.ref_a;
        ref_b = if ch_t = Ch_Ref_B then ch else chs.ref_b;
};;

(** Return sequence number of the packet *)
let get_packet_time (p : packet) =
    match p with
    | NoPacket -> 0
    | SnapshotPacket d -> d.sp_header.ph_packet_seq_num
    | IncRefreshPacket d -> d.rp_header.ph_packet_seq_num
;;

(** We need to compare between two packets here *)
let comp_packets (a, b : msg_source * msg_source) =
    match a.p, b.p with 
    | NoPacket, NoPacket -> a
    | NoPacket, _ -> b
    | _, NoPacket -> a
    | _, _ ->
    if get_packet_time(a.p) <= get_packet_time(b.p) then a else b
;;

(** We're using this instead of List.hd to avoid exceptions *)
let get_next_channel_packet_ref (ch : ref_channel_state) =
    if ch.r_unproc_packets = [] then NoPacket
    else IncRefreshPacket (List.hd ch.r_unproc_packets)
;;

let get_next_channel_packet_snap (ch : snap_channel_state) = 
    if ch.s_unproc_packets = [] then NoPacket 
    else SnapshotPacket (List.hd ch.s_unproc_packets)
;;

(** We need to select the next packet to parse from the 4 channels
    available to us. We will select the packet with the smallest
    sequence ID - i.e. the earliest one to arrive into the queue *)
let get_next_packet (ch : channels) =
    let pac_a = { p = get_next_channel_packet_ref (ch.ref_a); source = Ch_Ref_A} in
    let next_b = { p = get_next_channel_packet_ref (ch.ref_b); source = Ch_Ref_B} in
    let curr_min = comp_packets(pac_a, next_b) in
    let next_c = { p = get_next_channel_packet_snap (ch.snap_a); source = Ch_Snap_A} in
    let curr_min_c = comp_packets(curr_min, next_c) in
    let next_d = { p = get_next_channel_packet_snap (ch.snap_b); source = Ch_Snap_B }  in
    comp_packets (next_d, curr_min_c)
;;

(* *************************************************************** *)
(* Process the next packet when book state is RECOVERY             *)
(* *************************************************************** *)

(** Process snapshot packet when in recovery *)
let process_rec_snapshot (s, snap_p, src : feed_state * snap_packet * channel_type) = 
    (** This is a snapshot message. We need to do the following:
        1. Amend our liquidity state
        2. Save the snapshot if it's for our security. *)
    let last_snap = process_refresh_action (s.books, snap_p) in

    let new_snap = (
        if new_snapshot_better (last_snap, s.channels.last_snapshot) then
            Some last_snap
        else
            s.channels.last_snapshot) in

    (* We now need to update the cycle history *)
    let cycle_hist = get_cycle_hist (s.channels.cycle_hist_a, s.channels.cycle_hist_b, src) in
    let cycle_hist' = update_cycle_hist (cycle_hist, snap_p.sp_snap) in

    (** We're returning the exchange_state variable with updated channel info *)
    {
        s with
        channels = { s.channels with
            cycle_hist_a = if src = Ch_Snap_A then cycle_hist' else s.channels.cycle_hist_a;
            cycle_hist_b = if src = Ch_Snap_B then cycle_hist' else s.channels.cycle_hist_b;
            last_snapshot = new_snap;
        }
    }
;;

(** Process incremental refresh packet when in recovery *)
let process_rec_inc (s, ref_p : feed_state * ref_packet) = 
    (** If we're in recovery mode, then we need to add the message
        to the cache. We will process it if we consider cache complete. *)
    let cache' = s.channels.cache @ [ref_p.rp_msg] in
    { s with
        channels = { s.channels with cache = cache'};
    }
;;

let process_msg_recovery (s : feed_state) =
    let channels = s.channels in
    let next_packet = get_next_packet (s.channels) in
    let src = next_packet.source in

    match next_packet.p with
    | NoPacket -> s (* If there's nothing to process, then we simply return the current state *)

    | SnapshotPacket sp ->
        
        let this_ch = process_snap_ch ( get_snap_channel (channels, src)) in
        let channels' = set_snap_channel (channels, this_ch, src) in
        process_rec_snapshot ({ s with channels = channels' }, sp, src)

    | IncRefreshPacket rp ->
        let this_ch = process_ref_ch ( get_ref_channel (channels, src)) in
        let channels' = set_ref_channel (channels, this_ch, src) in

        process_rec_inc ({ s with channels = channels' }, rp)
;;

(** This will indicate that we need to reset the book and change to InRecovery *)
let is_msg_reset (msg : ref_message) =
    msg.rm_entry_type = V_MDEntryType_EmptyBook
;;

let msg_behind (msg, ch) = 
    msg.rm_rep_seq_num <= ch.last_seq_processed
;;

let msg_correct_seq (msg, ch) = 
    msg.rm_rep_seq_num = (ch.last_seq_processed + 1) 
;;

(** *************************************************************** *)
(** Process the next packet when book state is NORMAL               *)
(** *************************************************************** *)
let process_msg_normal (s : feed_state) =
    let channels = s.channels in
    let books = s.books in

    (** Since we're in a normal mode, we will simply move all of the
        packets that we've received here into processed  *)
    let channels' = {
        channels with
            snap_a = clean_snap_ch (channels.snap_a);
            snap_b = clean_snap_ch (channels.snap_b); } in

    let next_packet = get_next_packet (channels') in
    let src = next_packet.source in

    match next_packet.p with
        | NoPacket -> {s with channels = channels' }

        (* We shouldn't be processing these here *)
        | SnapshotPacket d -> {s with channels = channels' } 

        | IncRefreshPacket ref_packet ->

            let msg = ref_packet.rp_msg in
            let this_ch = process_ref_ch (get_ref_channel (channels', src)) in

            (** if it's the wrong security, just skip it *)
            if msg.rm_security_id <> s.sec_id then
                { s with channels = set_ref_channel (channels', this_ch, src); }

            else if msg_behind (msg, channels') then
                (** We have already processed this somewhere, so need to discard this message *)
                { s with channels = set_ref_channel (channels', this_ch, src); }
            
            else if msg_correct_seq (msg, channels') then
                (** This is the message that we need to process next *)
                if is_msg_reset (msg) then
                    { s with books = reset_books (s.books); feed_status = InRecovery; }
                
                else (
                    let books' = process_md_update_action (books, msg) in
                    let channels' =  set_ref_channel (channels', this_ch, src) in
                    { s with
                        books = books';
                        channels = { channels' with last_seq_processed = msg.rm_rep_seq_num };}
            )
            else
                (** We've detected a gap in message sequences *)
                let channels'' = { channels' with cache = channels'.cache @ [ msg ]; } in
                { s with
                    feed_status = InRecovery;
                    books = { s.books with b_status = Empty };
                    channels = set_ref_channel (channels'', this_ch, src);
                }
;;

(** *************************************************************** *)
(*  Recalculate the combined book based on the implied and the      *)
(*  multi-depth one                                                 *)
(** *************************************************************** *)
let recalc_combined (books : books) =
    let buys' = add_levels (sort_side (books.multi.buys @ books.implied.buys, BUY)) in
    let sells' = add_levels (sort_side (books.multi.sells @ books.implied.sells, SELL)) in
    let combined = {
        buys = trim_side (buys', books.book_depth, 1);
        sells = trim_side (sells', books.book_depth, 1);
    } in
    { books with combined = combined }
;;

(** Add an internal state change message to the list of the feed state *)
let add_int_message (s, ch_type : feed_state * book_change_type) =
{ s with internal_changes = s.internal_changes @ [mk_int_msg (s, ch_type)] }
;;

(*****************************************************************  *)
(** Top-level transition function                                   *)
(** *************************************************************** *)
let one_step (s : feed_state) =
    match s.feed_status with
    | InRecovery ->
        (** If we're in inconsistent state, then we need to listen to the recovery
            messages as well and attemp to recover the book (if we're liquid) *)
        let s' = process_msg_recovery (s) in
        let s'' = attempt_recovery (s') in

        (* If we've managed to get back to Normal state, then need to flag the
            transition *)
        if s''.feed_status = Normal then
            add_int_message (s'', Book_Changed_to_Normal)
        else if s''.channels.cache <> s.channels.cache then 
            add_int_message (s'', Book_Proc_Cache_Add)
        else
            s'

    | Normal ->
        (* 1. Get the earliest packet from either of the inc refresh *)
        (*      channels and process it *)
        (* 2. Since we're not listening to the snapshot channel, then *)
        (*      let's discard everything there *)
        (* 3. if we're still in the Normal state (after processing the message) *)
        let s' = process_msg_normal (s) in
        if s' <> s then (
            if s'.feed_status = Normal then
                let books' = recalc_combined (s'.books) in
                add_int_message ({s' with books = books'}, Book_Proc_Normal_Update)
            else
                add_int_message (s', Book_Changed_to_InRecovery)
        )
        else s'
;;

(** *************************************************************** *)
(**  Top-level transition function                                  *)
(** *************************************************************** *)
(*
   @meta[skip_proofs : simulate]
    function terminates
   @end
*)
let rec simulate(s : feed_state) =
    let s' = one_step s in
    if s' = s then
        s
    else
        begin
            let s' = {s' with cur_time = s'.cur_time + 1} in
            simulate(s')
        end
;;
