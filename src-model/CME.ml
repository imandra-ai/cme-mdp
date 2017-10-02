(**

    Aesthetic Integration Ltd.
    Copyright 2016

    CME.ml

*)

(* @meta[imandra_ignore] on @end *)
open CME_Types;;
(* @meta[imandra_ignore] off @end *)

let dec_units = 1000;;

(** Helper functions to convert between various *)
let dec_to_float (d) =
    (float_of_int (d / dec_units)) +. ((float_of_int (d mod dec_units)) /. (float_of_int dec_units));;

let float_to_dec (f) =
    int_of_float (f *. float_of_int(dec_units));;

(** We will use this type for setting property of our security *)
type liq_type = Liquid | Illiquid | LiqUnknown;;

(** These types represent the internal change book-keeping *)
type book_change_type =
    | Book_Changed_to_InRecovery    (* Changed status from Normal to InRecovery *)
    | Book_Changed_to_Normal        (* Changed stauts from InRecovery to Normal *)
    | Book_Proc_Normal_Update       (* Processed a packet in Normal mode, stayed in Normal *)
    | Book_Proc_Cache_Add           (* Processed a packet in InRecovery mode, stayed in InRecovery *)
    | Book_Proc_NotRelevant         (* Processed a packet that's not relevant (neither the current security nor the reference one) *)
    | Book_Proc_Snap                (* *)
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
let rec empty_order_levels (x:int) : order_level list =
    if x > 0 then NoLevel :: empty_order_levels (x - 1) else []
;;

(** Create the empty book for a given depth *)
(* Ask why it doesnt work
let empty_book (num_levels : int) = {
    buys  = empty_order_levels (num_levels);
    sells = empty_order_levels (num_levels);
};; *)

let empty_book (num_levels : int) = {
    buys  = [ NoLevel; NoLevel; NoLevel; NoLevel; NoLevel ];
    sells = [ NoLevel; NoLevel; NoLevel; NoLevel; NoLevel ];
};; 


(** ******************************************************* *)
(** This structure represents the history that we maintain  *)
(** ******************************************************* *)
type cycle_hist = {
    reference_sec_id : int;             (** Reference security *)
    self_sec_id : int;                  (** This security ID *)
    ref_sec_snap_received : bool;       (**  *)
    liq : liq_type;
};;

(** Structure with clean history *)
let clean_cycle_hist = {
   reference_sec_id = -1;
   self_sec_id = -1;
   ref_sec_snap_received = false;
   liq = LiqUnknown;
};;

(** We're encapsulating all of the messaging queues here *)
type channels = {
    unprocessed_packets : packet list;

    processed_messages : message list;
    processed_ref_a : packet list;
    processed_ref_b : packet list;
    processed_snap_a : packet list;
    processed_snap_b : packet list;

    cycle_hist_a : cycle_hist;      (* Cycle history for channel A *)
    cycle_hist_b : cycle_hist;      (* Cycle history for channel B *)

    last_seq_processed : int;       (* Last correctly processed sequence number *)
    cache : ref_message list;       (* These are cached incremental packets *)

    (** Last available snapshot -> updated only during recovery mode *)
    last_snapshot : snapshot option;
};;


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
    feed_sec_type : sec_type;
    feed_sec_id   : int;

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

let correct_level (msg, depth : ref_message * int) = 
    0 <= msg.rm_price_level && msg.rm_price_level <= depth 
;;

(*  Check whether the cache is complete: that we have all of the packets
    in the sequence starting with the last processed *)
let rec check_list (msgs, last_idx, book_depth : ref_message list * int * int) =
    match msgs with
    | [] -> true
    | x::xs ->
        if (x.rm_rep_seq_num = last_idx + 1) && correct_level(x, book_depth) then 
            check_list (xs, last_idx + 1, book_depth)
        else 
            false
;;

(* Check to see whether the cache we're maintaining is correct since the valid sequence number *)
let is_cache_valid_since_seq_num (cache, last_processed_seq, book_depth : ref_message list * int * int) =
    cache <> [] && check_list (cache, last_processed_seq, book_depth)
;;

(** Insert order into the book *)
let rec insert_order (a, s, orders : order_level * order_side * order_level list) =
  match orders with
  | [] -> [a]
  | x::xs ->
     if order_higher_ranked(s, a, x) then
       a::x::xs else x::insert_order(a, s, xs)
;;

(** Sort a single side of the book *)
let rec sort_side (orders, side : order_level list * order_side) =
    match orders with
    | [] -> []
    | x::xs -> insert_order (x, side, sort_side (xs, side))
;;

(** Top-level sorting function *)
let sort_book (b : book) = {
    buys = sort_side (b.buys, OrdBuy);
    sells = sort_side (b.sells, OrdSell);
};;

(** Remove all orders over num-levels *)
let rec trim_side ( ords, num_levels : order_level list * int ) = ords ;;

(*  NOTE: looks like we dont need this function at all
    if num_levels <= 0 then [] else
    match ords with
    | [] -> []
    | x::xs -> x::trim_side(xs, num_levels - 1)
;;
*)

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


(** *************************************************************** *)
(** Functions used for book modifications                           *)
(** *************************************************************** *)

(** Helper function -- replaces the book entry at the given level.
  * If the insertion violates sortedness of the book -- the change
  * is ignored (formally proven in imandra)
  * TODO: consider transitioning into recovery in case the inserion
  * is invalid.  *)
let rec replace_level_at (s, x, lst, n) = 
    match (n, lst) with
    | 2 , a::b::c::tl -> if order_higher_ranked(s,a,x) && order_higher_ranked(s,x,c) then a::x::c::tl else lst
    | 2 , a::b::[]    -> if order_higher_ranked(s,a,x) then a::x::[] else lst
    | 2 , a::[]       -> lst
    | 1 , a::b::tl    -> if order_higher_ranked(s,x,b) then x::b::tl else lst
    | 1 , a::[]       -> x::[]
    | n , a::tl       -> if n < 1 then lst else a::replace_level_at(s,x,List.tl lst,n-1)    
    | _ , []          -> []
;;

(** Used to remove orders from a list given its index. Appends NoLevel to the end of the list. *)
let rec delete_at (orders, n) =
    match orders with
    | [] -> []
    | h::tl -> 
        if n > 1 then h::delete_at(tl, n-1) 
        else if n = 1 then tl @ [NoLevel]
        else orders
;;

(** Delete from the book *)
let bk_delete (orders, price_level : order_level list * int) =
    delete_at (orders, price_level)
;;

(** Change an existing order level *)
let bk_change (orders, side, price_level, entry_size, entry_price, num_orders : order_level list * order_side * int * int * int * int option) =
    let new_order_info = Level {
        side = side;
        qty = entry_size;
        price = entry_price;
        num_orders = num_orders;
    } in 
    replace_level_at (side, new_order_info, orders, price_level)
;;

(** Add a new level to the book. Works only if the level is emty in the list *)
let bk_new (orders, side, price_level, entry_size, entry_price, num_orders : order_level list * order_side * int * int * int * int option) =
    let new_order_info = Level {
        side = side;
        qty = entry_size;
        price = entry_price;
        num_orders = num_orders;
    } in
    replace_level_at (side, new_order_info, orders, price_level)
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

let recalc_combined (books : books) =
    let buys' = add_levels (sort_side (books.multi.buys @ books.implied.buys, OrdBuy)) in
    let sells' = add_levels (sort_side (books.multi.sells @ books.implied.sells, OrdSell)) in
    let combined = {
        buys = trim_side (buys', books.book_depth);
        sells = trim_side (sells', books.book_depth);
    } in
    { books with combined = combined }
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
let reset_cycle_hist ( ch : cycle_hist ) = {
    clean_cycle_hist with reference_sec_id = ch.reference_sec_id
};;


(** *************************************************************** *)
(** Here we need to figure out what we're doing, etc.               *)
(** *************************************************************** *)
(** Security sequence number of the last message                    *)
(*
let process_refresh_action (books, snap : books * snap_message ) =
    let seq_num = snap.sm_snapshot.snap_last_msg_seq_num_processed in
    let depth = books.book_depth in
    let books' = {
        books with
            multi = empty_book (books.book_depth);
            implied = empty_book (books.book_depth);
            combined = empty_book (books.book_depth); } in
    let m = books'.multi in
    let m' = {
        buys = trim_side ( insert_order (snap.sm_real_bid, OrdBuy, m.buys), depth, 1);
        sells = trim_side (insert_order (snap.sm_real_ask, OrdSell, m.sells), depth, 1);
    } in
    let i = books'.implied in
    let i' = {
        buys = trim_side ( insert_order (snap.sm_imp_bid, OrdBuy, i.buys), depth, 1);
        sells = trim_side (insert_order (snap.sm_imp_ask, OrdSell, i.sells), depth, 1);
    } in {
       (** We're returning a snapshot message *)
        snap_m_book = m';
        snap_i_book = i';
        snap_seq_num = seq_num;
    }
;;
*)

(** Replace the book information with snapshot data *)
let apply_snapshot (books, snap : books * snapshot) =
    {
        books with
            multi = snap.snap_m_book;
            implied = snap.snap_i_book;
            combined = empty_book (books.book_depth);
    }
;;

(* :break *)
(** Is the latest snapshot better (later) than the existing one? *)
let new_snapshot_better (new_snap, old_snap : snapshot * snapshot option) =
    match old_snap with
        | None -> true
        | Some s ->
            new_snap.snap_last_msg_seq_num_processed >= s.snap_last_msg_seq_num_processed
;;

(** Change status of the books *)
let reset_books ( b : books ) =
{
    b with
    multi    = empty_book (b.book_depth);
    implied  = empty_book (b.book_depth);
    combined = empty_book (b.book_depth);
    b_status = Empty;
};;

(** Clean multi-depth book *)
let clean_multi_depth_book (books : books) =
    (* let book' = sort_book (books.multi) in *)
    let buys' = trim_side (books.multi.buys, books.book_depth) in
    let sells' = trim_side (books.multi.sells, books.book_depth) in
    {
        books with multi = { buys = buys'; sells = sells' }
    } 
;;

(* *************************************************************** *)
(*  Note that here we're assuming that the message is in sequence, *)
(*  hence we will not check sequence numbers here                  *)
(* *************************************************************** *)
let process_md_update_action (books, msg : books * ref_message) =
    let m = books.multi in
    let i = books.implied in

    match msg.rm_msg_type with
    | V_MDUpdateAction_New -> (
        match msg.rm_entry_type with
        | V_MDEntryType_Bid -> 
            let buys' = bk_new (m.buys, OrdBuy, msg.rm_price_level, msg.rm_entry_size,
                                    msg.rm_entry_px, msg.rm_num_orders) in
            let books' = { books with multi = { m with buys = buys'; }} in
            clean_multi_depth_book (books')
        | V_MDEntryType_Offer ->
            let sells' = bk_new (m.sells, OrdSell, msg.rm_price_level, msg.rm_entry_size,
                                      msg.rm_entry_px, msg.rm_num_orders) in
            let books' = { books with multi = { m with sells = sells'; }} in
            clean_multi_depth_book (books')
        | V_MDEntryType_ImpliedBid ->
            let buys' = bk_new (i.buys, OrdBuy, msg.rm_price_level, msg.rm_entry_size,
                                msg.rm_entry_px, None) in
            let books' = { books with implied = {i with buys = buys'; }} in
            clean_multi_depth_book (books')
        | V_MDEntryType_ImpliedOffer ->
            let sells' = bk_new (i.sells, OrdSell, msg.rm_price_level, msg.rm_entry_size,
                                 msg.rm_entry_px, None) in
            let books' = { books with implied = {i with sells = sells'; }} in
            clean_multi_depth_book (books')
        | _ -> books
    )

    | V_MDUpdateAction_Change -> (
        match msg.rm_entry_type with
        | V_MDEntryType_Bid ->
           let buys' = bk_change (m.buys, OrdBuy, msg.rm_price_level, msg.rm_entry_size,
                                  msg.rm_entry_px, msg.rm_num_orders)
           in { books with multi = { m with buys = buys' }}
        | V_MDEntryType_Offer ->
           let sells' = bk_change (m.sells, OrdSell, msg.rm_price_level, msg.rm_entry_size,
                                   msg.rm_entry_px, msg.rm_num_orders)
           in { books with multi = { m with sells = sells' }}
        | V_MDEntryType_ImpliedBid ->
           let buys' = bk_change (i.buys, OrdBuy, msg.rm_price_level, msg.rm_entry_size,
                                  msg.rm_entry_px, None)
           in { books with implied = { i with buys = buys' }}
        | V_MDEntryType_ImpliedOffer ->
           let sells' = bk_change (i.sells, OrdSell, msg.rm_price_level, msg.rm_entry_size,
                                   msg.rm_entry_px, None)
           in { books with implied = { i with sells = sells' }}
        | _ -> books
    )

    | V_MDUpdateAction_Delete -> (
        (** Here we only care about the level that we're deleting, also need to make *)
        (** sure the size is filled in correctly with NoLevel objects after the delete is done *)
        match msg.rm_entry_type with
        | V_MDEntryType_Bid ->
            let buys' = bk_delete (m.buys, msg.rm_price_level)
            in { books with multi = { m with buys = buys' } }
        | V_MDEntryType_Offer ->
            let sells' = bk_delete (m.sells, msg.rm_price_level)
            in { books with multi = { m with sells = sells' } }
        | V_MDEntryType_ImpliedBid ->
            let buys' = bk_delete (i.buys, msg.rm_price_level)
            in { books with implied = { i with buys = buys' } }
        | V_MDEntryType_ImpliedOffer ->
            let sells' = bk_delete (i.sells, msg.rm_price_level)
            in { books with implied = { i with sells = sells' } }
        | _ -> books
    )

    | V_MDUpdateAction_DeleteThru -> (
        match msg.rm_entry_type with
        (** We're getting rid of all of the orders on one side here *)
        | V_MDEntryType_Bid -> { books with multi = { m with buys = []; }}
        | V_MDEntryType_Offer -> { books with multi = { m with sells = []; }}
        | V_MDEntryType_ImpliedBid -> { books with implied = { i with buys = []; }}
        | V_MDEntryType_ImpliedOffer -> { books with implied = { i with sells = []; }}
        | _ -> books
    )

    | V_MDUpdateAction_DeleteFrom -> (
        (** Delete all orders from a side from a given level *)
        match msg.rm_entry_type with
        | V_MDEntryType_Bid ->
            let buys' = bk_delete (m.buys, msg.rm_price_level) in
            { books with multi = { m with buys = buys' } }
        | V_MDEntryType_Offer ->
            let sells' = bk_delete (m.sells, msg.rm_price_level) in
            { books with multi = { m with sells = sells' } }
        | V_MDEntryType_ImpliedBid ->
            let buys' = bk_delete (i.buys, msg.rm_price_level) in
            { books with implied = { i with buys = buys' } }
        | V_MDEntryType_ImpliedOffer ->
            let sells' = bk_delete (i.sells, msg.rm_price_level) in
            { books with implied = { i with sells = sells' } }
        | _ -> books
    )

    | _ -> books (** We only care about the update actions here *)
;;
(** *************************************************************** *)

let rec apply_update_packets (books, packets : books * ref_message list) =
    match packets with
    | [] -> books
    | x::xs -> let books' = process_md_update_action (books, x)
                in apply_update_packets (books', xs)
;;

let apply_cache (books, channels : books * channels) =
    apply_update_packets (books, channels.cache)
;;

(** Add an internal state change message to the list of the feed state *)
let add_int_message (s, ch_type : feed_state * book_change_type) =
{ s with internal_changes = s.internal_changes @ [mk_int_msg (s, ch_type)] }
;;

(**
    Check whether the feed can now fully recover.
    There are two cases:

    -- The cache is now valid -> it has the full sequence of packets since the
        last processed one

    -- We have a snapshot and a full sequence of packets since the snapshot.

    Alternatively, we have an illiquid security, hence we need to use the
    incremental refresh channels only.
*)
let attempt_recovery (s : feed_state) =
    let books = s.books in
    let channels = s.channels in
    if channels.cycle_hist_a.liq = Illiquid && channels.cycle_hist_b.liq = Illiquid then
        let books' = apply_cache (books, channels) in
        let s' = { s with feed_status = Normal; books = books'; } in 
        add_int_message (s', Book_Changed_to_Normal)
    else (
        if is_cache_valid_since_seq_num (channels.cache, channels.last_seq_processed, s.books.book_depth) then (
            let books' = apply_cache (books, channels) in
            let books'' = recalc_combined (books') in 
            let s' = { s with
                        feed_status = Normal;
                        books = books'';
                        channels = { 
                            channels with cache = [] 
                        }
                    } in 
            add_int_message (s', Book_Changed_to_Normal)
        )

        else match channels.last_snapshot with
            | None -> s
            | Some snap ->
                if is_cache_valid_since_seq_num (
                        channels.cache, 
                        snap.snap_last_msg_seq_num_processed, 
                        s.books.book_depth
                    ) then
                    let books' = apply_snapshot (books, snap) in
                    let books'' = apply_cache (books', channels) in
                    let books''' = recalc_combined (books'') in 
                    let s' =    { s 
                                with books = books''';
                                    feed_status = Normal;
                                    channels = { 
                                        channels with cache = [] 
                                }} in 
                    add_int_message (s', Book_Changed_to_Normal)
                else
                    s
        )
;;

(** Return sequence number of the packet *)
let get_packet_time (p : packet) = p.packet_seq_num;;

(** We need to compare between two packets here *)
let comp_packets (a, b : packet * packet) =
    if get_packet_time a <= get_packet_time b then a else b
;;

(* *************************************************************** *)
(* Process the next packet when book state is RECOVERY             *)
(* *************************************************************** *)

(** Process snapshot message when in recovery *)
let process_rec_snapshot (s, snap, src : feed_state * snap_message * channel_type) = 
    (** This is a snapshot message. We need to do the following:
        1. Amend our liquidity state
        2. Save the snapshot if it's for our security. *)
    let new_snap = snap.sm_snapshot in
    let new_snap = (
        if new_snapshot_better (new_snap, s.channels.last_snapshot) then
            Some new_snap
        else
            s.channels.last_snapshot) in

    (* We now need to update the cycle history *)
    let cycle_hist = get_cycle_hist (s.channels.cycle_hist_a, s.channels.cycle_hist_b, src) in
    let cycle_hist' = update_cycle_hist (cycle_hist, snap ) in

    (** We're returning the exchange_state variable with updated channel info *)
    {
        s with
        channels = { s.channels with
            cycle_hist_a = if src = Ch_Snap_A then cycle_hist' else s.channels.cycle_hist_a;
            cycle_hist_b = if src = Ch_Snap_B then cycle_hist' else s.channels.cycle_hist_b;
            last_seq_processed = ( match new_snap with
                | None  -> s.channels.last_seq_processed
                | Some new_snap ->  new_snap.snap_last_msg_seq_num_processed ) ;
            last_snapshot = new_snap;
        }
    }
;;

(** Need to add to the cache in the right order *)
let rec add_to_cache (msg, cache : ref_message * ref_message list) = 
    match cache with 
    | [] -> [msg]    
    | [x] ->
        if x.rm_rep_seq_num < msg.rm_rep_seq_num then
            cache @ [msg]
        else if x.rm_rep_seq_num = msg.rm_rep_seq_num then 
            cache
        else
            msg::cache

    | x::y::xs ->
        if x.rm_rep_seq_num = msg.rm_rep_seq_num || y.rm_rep_seq_num = msg.rm_rep_seq_num then 
            cache
        else if x.rm_rep_seq_num > msg.rm_rep_seq_num then
            msg :: cache
        else if  x.rm_rep_seq_num < msg.rm_rep_seq_num && msg.rm_rep_seq_num < y.rm_rep_seq_num then
            x::msg::y::xs
        else 
            x :: add_to_cache (msg, y::xs)
;;

(** Auxillary check used for proofs *)
let rec is_cache_sorted (cache : ref_message list) =
    match cache with 
    | [] -> true
    | [x] -> true
    | x::y::xs ->
        if x.rm_rep_seq_num >= y.rm_rep_seq_num then
            false
        else
            is_cache_sorted(y::xs)
;;


let process_msg_recovery (s, next_message, channel_type  : feed_state * message * channel_type) =
    match next_message with
    | SnapshotMessage sm ->        
        let s = process_rec_snapshot ( s, sm, channel_type) in 
        add_int_message  (s, Book_Proc_Snap)
    | RefreshMessage rm ->
        if rm.rm_security_id <> s.feed_sec_id then
            add_int_message (s, Book_Proc_NotRelevant)
        else
            let cache = add_to_cache (rm , s.channels.cache) in
            let s = { s with channels = { s.channels with cache = cache} } in 
            add_int_message (s, Book_Proc_Cache_Add)
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

let is_msg_relevant (msg, s : ref_message * feed_state) = 
    if msg.rm_security_id <> s.feed_sec_id || 
        msg_behind (msg, s.channels) || 
        not (correct_level (msg, s.books.book_depth))  then false
    else true
;;


(** *************************************************************** *)
(** Process the next packet when book state is NORMAL               *)
(** *************************************************************** *)
(* 1. Get the earliest packet from either of the inc refresh *)
(*      channels and process it *)
(* 2. Since we're not listening to the snapshot channel, then *)
(*      let's discard everything there *)
(* 3. if we're still in the Normal state (after processing the message) *)
let process_msg_normal ( s , next_message : feed_state * message ) =
   match next_message with
        | SnapshotMessage _ -> 
            add_int_message ( s , Book_Proc_NotRelevant )
        | RefreshMessage rm ->
            (** if it's the wrong security, just skip it *)
            if not (is_msg_relevant (rm, s)) then 
                add_int_message (s, Book_Proc_NotRelevant)

            else if msg_correct_seq (rm, s.channels) then begin
                (** This is the message that we need to process next *)
                let s = { s with 
                    channels = { s.channels with last_seq_processed = rm.rm_rep_seq_num } } in
                if is_msg_reset rm then
                    let s = { s with 
                        books = reset_books (s.books);
                        feed_status = InRecovery; 
                    } in 
                    add_int_message (s, Book_Changed_to_InRecovery)
                else 
                    let books = process_md_update_action (s.books, rm) in
                    let books = recalc_combined books in
                    let s = { s with books = books; } 
                in
                add_int_message (s, Book_Proc_Normal_Update)
            end 
            else
                (** We've detected a gap in message sequences *)
                let channels = { s.channels with cache = s.channels.cache @ [ rm ]; } in
                let s = { s with
                    feed_status = InRecovery;
                    books = { s.books with b_status = Empty };
                    channels = channels;
                } in 
                add_int_message (s, Book_Changed_to_InRecovery)
;;



(* Helper for the top-level transition *)
let move_to_next_packet ( s, current_packet, rest_packets : feed_state * packet * packet list )  =
    let c = s.channels in
    let processed = { current_packet with packet_messages = List.rev c.processed_messages } in
    let c = match current_packet.packet_channel with
        | Ch_Ref_A  -> { c with processed_ref_a  = c.processed_ref_a  @ [processed] }
        | Ch_Ref_B  -> { c with processed_ref_b  = c.processed_ref_b  @ [processed] }
        | Ch_Snap_A -> { c with processed_snap_a = c.processed_snap_a @ [processed] }
        | Ch_Snap_B -> { c with processed_snap_b = c.processed_snap_b @ [processed] }
        in
    let c = { c with unprocessed_packets = rest_packets; processed_messages = [] } in
    { s with channels = c }
;;


let get_next_packet (s : feed_state ) =
    match s.channels.unprocessed_packets with [] -> None | current_packet::rest_packets ->
    Some current_packet
;;

let get_next_message (s : feed_state ) =
    match s.channels.unprocessed_packets with [] -> None | current_packet::rest_packets ->
    match current_packet.packet_messages with [] -> None | current_message::rest_messages ->
    Some current_message
;;

(*****************************************************************  *)
(** Top-level transition function                                   *)
(** *************************************************************** *)
let one_step (s : feed_state) =
    (* If there's no packet to process, then we simply return the current state *)
    match s.channels.unprocessed_packets with [] -> s | current_packet::rest_packets ->
    match current_packet.packet_messages with 
        (* If there's no message to process, then we shift the current packet *)
        | [] -> move_to_next_packet (s, current_packet, rest_packets )
        (* If there's a message to process, then we can process it *)
        | current_message::rest_messages ->
        let s = match s.feed_status with
            | InRecovery ->
                (** If we're in inconsistent state, then we need to listen to the recovery
                    messages as well and attemp to recover the book (if we're liquid) *)
                let s' = process_msg_recovery ( s, current_message, current_packet.packet_channel ) in
                attempt_recovery (s')
            | Normal -> process_msg_normal (s, current_message)
            in
        let channels = { s.channels with  
            unprocessed_packets = {current_packet with packet_messages = rest_messages}::rest_packets;
            processed_messages  = current_message::s.channels.processed_messages
            } in
        { s with channels = channels}
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

let empty_feed_state : feed_state =
  { feed_sec_type = SecA
  ; feed_sec_id   = 1 (* TODO Fix the secid shizm*)
  ; books =
      { book_depth = 5
      ; multi      = empty_book 5
      ; implied    = empty_book 5
      ; combined   = empty_book 5
      ; b_status   = Empty
      }
  ; channels =
      { unprocessed_packets = []
      ; processed_messages = []
      ; processed_ref_a = []
      ; processed_ref_b = []
      ; processed_snap_a = []
      ; processed_snap_b = []
      ; cycle_hist_a = clean_cycle_hist
      ; cycle_hist_b = clean_cycle_hist
      ; last_seq_processed = 0
      ; cache = []
      ; last_snapshot = None
      }
  ; feed_status = Normal
  ; internal_changes = []
  ; cur_time = 0
  }
;;
