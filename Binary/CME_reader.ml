#use "topfind";;
#require "bitstring";;

#directory "../_build/binary";;
#load "binparser.cmo";;

#directory "../_build/out_types";;
#load "message_types.cmo";;
#load "readers.cmo";;

#directory "../_build/model";;
#load "CME.cmo";;

let pricenull_to_int p =
    let m = p.Message_types.f_PRICENULL_mantissa |> ( function Some x -> Int64.to_int x | None -> 0 ) in
    let e = p.Message_types.f_PRICENULL_exponent in
    let rec p10 = function 0 -> 1 | n -> 10 * p10 (n-1) in
    m / ( p10 (-e) / CME.dec_units)

let inc_refresh entry = 
    let conv_msg_type = function
        | Message_types.V_MDUpdateAction_New         -> CME.V_MDUpdateAction_New
        | Message_types.V_MDUpdateAction_Change      -> CME.V_MDUpdateAction_Change
        | Message_types.V_MDUpdateAction_Delete      -> CME.V_MDUpdateAction_Delete
        | Message_types.V_MDUpdateAction_DeleteThru  -> CME.V_MDUpdateAction_DeleteThru
        | Message_types.V_MDUpdateAction_DeleteFrom  -> CME.V_MDUpdateAction_DeleteFrom
        | Message_types.V_MDUpdateAction_Overlay     -> CME.V_MDUpdateAction_Overlay    
    in 
    let conv_entry_type = function
        | Message_types.V_MDEntryTypeBook_Bid                 -> CME.V_MDEntryType_Bid
        | Message_types.V_MDEntryTypeBook_Offer               -> CME.V_MDEntryType_Offer
        | Message_types.V_MDEntryTypeBook_ImpliedBid          -> CME.V_MDEntryType_ImpliedBid
        | Message_types.V_MDEntryTypeBook_ImpliedOffer        -> CME.V_MDEntryType_ImpliedOffer
        | Message_types.V_MDEntryTypeBook_BookReset           -> CME.V_MDEntryType_EmptyBook  (* Note @ Denis !*)
    in
    {
        CME.rm_security_id = entry.Message_types.f_MDIncrementalRefreshBook32_NoMDEntries_SecurityID   |> Int32.to_int;
        CME.rm_rep_seq_num = entry.Message_types.f_MDIncrementalRefreshBook32_NoMDEntries_RptSeq       |> Int32.to_int;
        CME.rm_price_level = entry.Message_types.f_MDIncrementalRefreshBook32_NoMDEntries_MDPriceLevel;
        CME.rm_entry_size  = entry.Message_types.f_MDIncrementalRefreshBook32_NoMDEntries_MDEntrySize  |> (function Some x -> Int32.to_int x | None -> 0 );
        CME.rm_entry_px    = entry.Message_types.f_MDIncrementalRefreshBook32_NoMDEntries_MDEntryPx |> pricenull_to_int ;
        CME.rm_num_orders  = entry.Message_types.f_MDIncrementalRefreshBook32_NoMDEntries_NumberOfOrders |> (function Some x -> Int32.to_int x | None -> 0 );
        CME.rm_msg_type    = conv_msg_type entry.Message_types.f_MDIncrementalRefreshBook32_NoMDEntries_MDUpdateAction;
        CME.rm_entry_type  = conv_entry_type entry.Message_types.f_MDIncrementalRefreshBook32_NoMDEntries_MDEntryType
    };;

let snap_refresh msg = 
    let open CME in 
    let sm_last_msg_seq_num_processed = msg.Message_types.f_SnapshotFullRefresh38_LastMsgSeqNumProcessed |> Int32.to_int in
    let sm_security_id                = msg.Message_types.f_SnapshotFullRefresh38_SecurityID             |> Int32.to_int in
    let sm_rep_seq_num                = msg.Message_types.f_SnapshotFullRefresh38_RptSeq                 |> Int32.to_int in
    let convert_offer m = Level {
        side = BUY; (* <- that is wrong*)
        qty        = m.Message_types.f_SnapshotFullRefresh38_NoMDEntries_MDEntrySize    |> (function Some x -> Int32.to_int x | None -> 0 );
        price      = m.Message_types.f_SnapshotFullRefresh38_NoMDEntries_MDEntryPx      |> pricenull_to_int;
        num_orders = m.Message_types.f_SnapshotFullRefresh38_NoMDEntries_NumberOfOrders |> (function Some x -> Some (Int32.to_int x) | None -> None );
    } in
    let rec scan (rb, ra, ib, ia) = function
        | [] -> (rb, ra, ib, ia)
        | h::t -> ( match h.Message_types.f_SnapshotFullRefresh38_NoMDEntries_MDEntryType with
            | V_MDEntryType_Bid          -> scan ( convert_offer h , ra , ib , ia ) t
            | V_MDEntryType_Offer        -> scan ( rb , convert_offer h , ib , ia ) t
            | V_MDEntryType_ImpliedBid   -> scan ( rb , ra , convert_offer h , ia ) t
            | V_MDEntryType_ImpliedOffer -> scan ( rb , ra , ib , convert_offer h ) t 
    ) in 
    let ( sm_real_bid , sm_real_ask , sm_imp_bid , sm_imp_ask ) = 
        scan (NoLevel, NoLevel, NoLevel, NoLevel) msg.Message_types.f_NoMDEntries in
    {
        sm_last_msg_seq_num_processed;
        sm_security_id; sm_rep_seq_num;               
        sm_real_bid; sm_real_ask;
        sm_imp_bid ; sm_imp_ask ;
    } ;;

let read_chanels bits nmsgs =
   let rec read bits (ir, snp) n =
        let ( md, payld, bits ) = Binparser.get_message bits in 
        if n = nmsgs then (ir,snp) else
        match Readers.read_message payld md with
            | M_MDIncrementalRefreshBook32 m -> read bits (ir @ List.map inc_refresh m.f_NoMDEntries , snp) (n + 1)
            | M_SnapshotFullRefresh38 m -> read bits ( ir , snp @ [snap_refresh m] ) (n + 1)
            | _ -> read bits (ir,snp) (n + 1)
    in
    read bits ([],[]) 0
                
let bits = Binparser.open_in "../../samples/224.0.31.130_14382_A_20151027";;

let ir, snp = read_chanels bits 2000 ;;
