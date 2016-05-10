(**    

    Aesthetic Integration Ltd.
    Copyright 2016
    
    CME_reader.ml
    
*)

let pricenull_to_int p =
    let ex = float_of_int p.Message_types.f_PRICENULL_exponent in
    match p.Message_types.f_PRICENULL_mantissa with 
    | None   -> 0
    | Some x -> ((Int64.to_float x) *. ( 10. ** ex ) ) |> int_of_float

let int32_opt_to_int = function  
    | None   -> 0
    | Some x -> Int32.to_int x 
    
let int32_opt_to_int_opt = function  
    | None -> None 
    | Some x -> Some ( Int32.to_int x ) 

        
(*                               *)
(*    Incremental refresh part   *)
(*                               *)

let inc_refresh msg = 
    let open Message_types in
    let conv_msg_type = function
        | V_MDUpdateAction_New        -> CME.Action_New         
        | V_MDUpdateAction_Change     -> CME.Action_Change      
        | V_MDUpdateAction_Delete     -> CME.Action_Delete      
        | V_MDUpdateAction_DeleteThru -> CME.Action_DeleteThru  
        | V_MDUpdateAction_DeleteFrom -> CME.Action_DeleteFrom  
        | V_MDUpdateAction_Overlay    -> CME.Action_Overlay     
    in 
    let conv_entry_type = function
        | V_MDEntryTypeBook_Bid          -> CME.Entry_Bid                  
        | V_MDEntryTypeBook_Offer        -> CME.Entry_Offer                 
        | V_MDEntryTypeBook_ImpliedBid   -> CME.Entry_ImpliedBid            
        | V_MDEntryTypeBook_ImpliedOffer -> CME.Entry_ImpliedOffer          
        | V_MDEntryTypeBook_BookReset    -> CME.Entry_EmptyBook             
    in
    {
        CME.rm_security_id = msg.f_MDIncrementalRefreshBook32_NoMDEntries_SecurityID      |> Int32.to_int;
        CME.rm_rep_seq_num = msg.f_MDIncrementalRefreshBook32_NoMDEntries_RptSeq          |> Int32.to_int;
        CME.rm_price_level = msg.f_MDIncrementalRefreshBook32_NoMDEntries_MDPriceLevel    ;
        CME.rm_entry_size  = msg.f_MDIncrementalRefreshBook32_NoMDEntries_MDEntrySize     |> int32_opt_to_int;
        CME.rm_entry_px    = msg.f_MDIncrementalRefreshBook32_NoMDEntries_MDEntryPx       |> pricenull_to_int;
        CME.rm_num_orders  = msg.f_MDIncrementalRefreshBook32_NoMDEntries_NumberOfOrders  |> int32_opt_to_int;
        CME.rm_msg_type    = msg.f_MDIncrementalRefreshBook32_NoMDEntries_MDUpdateAction  |> conv_msg_type;
        CME.rm_entry_type  = msg.f_MDIncrementalRefreshBook32_NoMDEntries_MDEntryType     |> conv_entry_type 
    }
    
let read_ref_packets md msg =
    let common_header = { 
            CME.ph_packet_seq_num = Int32.to_int md.Binparser.seqence_num;
            CME.ph_sending_time   = Int64.to_int md.Binparser.sent_ts;
    } in
    let entry_to_packet entry = {
        CME.rp_header = common_header;
        CME.rp_msg = inc_refresh entry
    } in
    List.map entry_to_packet msg.Message_types.f_MDIncrementalRefreshBook32_NoMDEntries
    
(*                               *)
(*    Snapshot part              *)
(*                               *)

let snap_refresh msg =
    let open Message_types in
    let mk_books entries =
        let rec scan (buys, sells, ibuys, isells) = function 
            | [] -> (buys, sells, ibuys, isells)
            | entry::tl -> 
                let order = CME.Level { 
                CME.price = entry.f_SnapshotFullRefresh38_NoMDEntries_MDEntryPx   |> pricenull_to_int;
                CME.qty   = entry.f_SnapshotFullRefresh38_NoMDEntries_MDEntrySize |> int32_opt_to_int;
                CME.num_orders = entry.f_SnapshotFullRefresh38_NoMDEntries_NumberOfOrders |> int32_opt_to_int_opt;
                CME.side  = match entry.f_SnapshotFullRefresh38_NoMDEntries_MDEntryType with  
                    | V_MDEntryType_Bid   | V_MDEntryType_ImpliedBid   -> CME.BUY
                    | V_MDEntryType_Offer | V_MDEntryType_ImpliedOffer -> CME.SELL
                    | _ ->  CME.SELL
                } in
                match entry.f_SnapshotFullRefresh38_NoMDEntries_MDEntryType with     
                    | V_MDEntryType_Bid          -> scan (order::buys, sells, ibuys, isells) tl 
                    | V_MDEntryType_Offer        -> scan (buys, order::sells, ibuys, isells) tl
                    | V_MDEntryType_ImpliedBid   -> scan (buys, sells, order::ibuys, isells) tl
                    | V_MDEntryType_ImpliedOffer -> scan (buys, sells, ibuys, order::isells) tl
                    | _ -> scan (buys, sells, ibuys, isells) tl
            in
        let (buys, sells, ibuys, isells) =  scan ([],[],[],[]) entries in
        {CME.buys =  buys; CME.sells =  sells}, 
        {CME.buys = ibuys; CME.sells = isells} 
    in
    let book, implied_book = mk_books msg.f_SnapshotFullRefresh38_NoMDEntries in
    let last_seq = msg.f_SnapshotFullRefresh38_LastMsgSeqNumProcessed |>  Int32.to_int in
    {
        CME.sm_last_msg_seq_num_processed = last_seq;
        CME.sm_security_id = msg.f_SnapshotFullRefresh38_SecurityID |>  Int32.to_int ;
        CME.sm_rep_seq_num = msg.f_SnapshotFullRefresh38_RptSeq     |>  Int32.to_int ;
        CME.sm_snapshot = {
            CME.snap_seq_num = last_seq; 
            CME.snap_m_book = book;
            CME.snap_i_book = implied_book
        }
    }

let read_snap_packet md msg = {
        CME.sp_header = {
            CME.ph_packet_seq_num = Int32.to_int md.Binparser.seqence_num;
            CME.ph_sending_time   = Int64.to_int md.Binparser.sent_ts;        
        };
        CME.sp_snap = snap_refresh msg
    }::[] 
    
let read_packets channel md =
    match Readers.read_message channel md with
        | Message_types.M_MDIncrementalRefreshBook32 m -> 
            read_ref_packets md m |> List.map (fun p -> CME.IncRefreshPacket p)
        | Message_types.M_SnapshotFullRefresh38 m -> 
            read_snap_packet md m |> List.map (fun p -> CME.SnapshotPacket   p)
        | _ -> []
