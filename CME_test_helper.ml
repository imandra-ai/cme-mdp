(* @meta[imandra_ignore] on @end *)

open CME;;

(* @meta[imandra_ignore] off @end *)


let empty_channels = {
    ref_a = {
        r_unproc_packets = [];
        r_proc_packets = [];
    };

    ref_b = {
        r_unproc_packets = [];
        r_proc_packets = [];
    };

    last_seq_processed = 0;
    cache = [];

    last_snapshot = None;

    (** Refresh channels that we subscribe to only when in recovery mode *)
    snap_a = {
        s_unproc_packets = [];
        s_proc_packets = [];
    };

    cycle_hist_a = {
        reference_sec_id = 1;
        self_sec_id = 2;
        ref_sec_snap_received = false;
        liq = LiqUnknown;
    };

    snap_b = {
        s_unproc_packets = [];
        s_proc_packets = [];
    };

    cycle_hist_b = {
        reference_sec_id = 1;
        self_sec_id = 2;
        ref_sec_snap_received = false;
        liq = LiqUnknown;
    };

};;

(** The following two functions are used to tranform internal packet list types to those
    we would see in actual protocol. See the note above. *)
let rec merge_ref_packets ( old_p, new_p : ref_packet list * ref_packet_lst list ) =
    match old_p with
    | [] -> new_p
    | x::xs ->
        let new_p' = (
            match new_p with
                | [] -> [ { rpl_header = x.rp_header;
                            rpl_msgs = [ x.rp_msg ] }]
                | y::ys ->
                    if x.rp_header = y.rpl_header then
                        (** in this case we just append it to the list of current ones *)
                        { y with rpl_msgs = y.rpl_msgs @ [ x.rp_msg ] } :: ys
                    else
                        let new_pl = { rpl_header = x.rp_header; rpl_msgs = [ x.rp_msg ]} in
                        new_pl :: new_p
        )
    in
        merge_ref_packets (xs, new_p')
;;

(** merge_snap_packets: tranforms list of individual packets into
    snap_packet list -> snap_packet_lst *)
let rec merge_snap_packets ( old_p, new_p : snap_packet list * snap_packet_lst list) =
    match old_p with
    | [] -> new_p
    | x::xs ->
        let new_p' = (
            match new_p with
                | [] -> [ { spl_header = x.sp_header;
                            spl_snap = [ x.sp_snap ]}; ]
                | y::ys ->
                    if x.sp_header = y.spl_header then
                        { y with spl_snap = y.spl_snap @ [ x.sp_snap ]} :: ys
                    else
                        let new_pl = { spl_header = x.sp_header; spl_snap = [ x.sp_snap ]} in
                        new_pl :: new_p
        )
    in
        merge_snap_packets (xs, new_p')
;;

(** Create a list of internal snapshot messages *)
let rec explode_snap_packets (old_p, new_p : snap_packet_lst list * snap_packet list) =
    match old_p with
    | [] -> new_p
    | x::xs ->
        match x.spl_snap with
            | [] -> explode_snap_packets (xs, new_p)
            | y::ys ->
                let new_p' = { sp_header = x.spl_header; sp_snap = y } in
                let x' = { x with spl_snap = ys } in
                explode_snap_packets (x'::xs, new_p'::new_p)
;;

(** And do the same for incremental refresh packets *)
let rec explode_ref_packets (old_p, new_p : ref_packet_lst list * ref_packet list) =
    match old_p with
    | [] -> new_p
    | x::xs ->
        match x.rpl_msgs with
            | [] -> explode_ref_packets (xs, new_p)
            | y::ys ->
                let new_p' = { rp_header = x.rpl_header; rp_msg = y; } in
                let x' = { x with rpl_msgs = ys } in
                explode_ref_packets (x'::xs, new_p'::new_p)
;;

let rec get_ref_packets (p, ref_only : packet list * ref_packet list) =
    match p with
    | [] -> ref_only
    | x::xs ->
        match x with
            | IncRefreshPacket d -> get_ref_packets( xs, ref_only @ [ d ] )
            | _ -> get_ref_packets ( xs, ref_only )
;;

let rec get_snap_packets (p, snap_only : packet list * snap_packet list) =
    match p with
    | [] -> snap_only
    | x::xs ->
        match x with
            | SnapshotPacket d -> get_snap_packets( xs, snap_only @ [ d ] )
            | _ -> get_snap_packets ( xs, snap_only )
;;

let rec gen_side_orders (side_orders, curr_idx, seq_num, is_implied, s : order_level list * 
                            int * int * bool * feed_state) = 
    match side_orders with 
    | [] -> []
    | x::xs ->
        match x with 
        | NoLevel -> []
        | Level l -> 
        let ent_type = 
            match l.side, is_implied with 
            | BUY, false    -> V_MDEntryType_Bid
            | BUY, true     -> V_MDEntryType_ImpliedBid
            | SELL, false   -> V_MDEntryType_Offer
            | SELL, true    -> V_MDEntryType_ImpliedOffer in 
        {  
            rm_security_id  = s.sec_id;
            rm_rep_seq_num  = seq_num + 1;
            rm_msg_type     = V_MDUpdateAction_New;
            rm_entry_type   = ent_type;
            rm_price_level  = curr_idx;
            rm_entry_size   = l.qty;
            rm_entry_px     = l.price;
            rm_num_orders   = 
                match l.num_orders with 
                | None -> 0
                | Some d -> d;
        } :: gen_side_orders (xs, curr_idx + 1, seq_num + 1, is_implied, s)
;;

let rec get_last_seq_num ( msgs, last_seq : ref_message list * int ) = 
    match msgs with 
    | [] -> last_seq
    | [y] -> y.rm_rep_seq_num
    | x::xs -> get_last_seq_num (xs, last_seq)
;;

(** Create a packet to reconstruct the book as messages *)
let book_to_messages (s : feed_state) =
    let b = s.books in 
    let seq_num = s.channels.last_seq_processed in 
    let buy_packets_m   = gen_side_orders (b.multi.buys,    1, seq_num, false,  s) in 
    let seq_num         = get_last_seq_num (buy_packets_m, seq_num) in 
    let sell_packets_m  = gen_side_orders (b.multi.sells,   1, seq_num, false,  s) in
    let seq_num         = get_last_seq_num (sell_packets_m, seq_num) in  
    let buy_packets_i   = gen_side_orders (b.implied.buys,  1, seq_num, true,   s) in
    let seq_num         = get_last_seq_num (buy_packets_i, seq_num) in  
    let sell_packets_i  = gen_side_orders (b.implied.sells, 1, seq_num, true,   s) in
    [{
        rpl_header = {
            ph_packet_seq_num = 1;
            ph_sending_time = 1;
        }; 

        rpl_msgs = 
            (List.rev buy_packets_m) @ 
            (List.rev sell_packets_m) @ 
            (List.rev buy_packets_i) @ 
            (List.rev sell_packets_i);
    }] 
;;


let rec create_packets_ref ( ref_p : ref_packet list ) = 
    match ref_p with 
    | [] -> []
    | x::xs -> IncRefreshPacket x :: create_packets_ref (xs)
;;

let rec create_packets_snap ( snap_p : snap_packet list ) =
    match snap_p with 
    | [] -> []
    | x::xs -> SnapshotPacket x :: create_packets_snap (xs)
;;


(** Create a single stream of packets *)
let rec merge_messages (ch : channels) = 
    let n_p = get_next_packet (ch) in 
    match n_p.p with 
    | NoPacket -> []
    | SnapshotPacket _ -> 
        let this_ch = process_snap_ch ( get_snap_channel (ch, n_p.source)) in
        let ch' = set_snap_channel (ch, this_ch, n_p.source) in
        n_p.p :: merge_messages (ch')

    | IncRefreshPacket rp ->
        let this_ch = process_ref_ch ( get_ref_channel (ch, n_p.source)) in
        let ch' = set_ref_channel (ch, this_ch, n_p.source) in
        n_p.p :: merge_messages (ch')
;;










