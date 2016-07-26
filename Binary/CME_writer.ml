(**    

    Aesthetic Integration Ltd.
    Copyright 2016
    
    CME_writer.ml
    
*)

let int_to_pricenull i =
    let m = match i with 
        | 0 -> None 
        | x -> Some (Int64.of_int (x * ( 10000000 / CME.dec_units ) ) ) 
    in { Message_types.f_PRICENULL_mantissa =  m;
         Message_types.f_PRICENULL_exponent = -7; }

let int_to_int32_opt = function  
    | 0 -> None 
    | x -> Some ( Int32.of_int x ) 
    
let int_opt_to_int32_opt = function  
    | None -> None 
    | Some x -> Some ( Int32.of_int x ) 

(*                               *)
(*    Incremental refresh part   *)
(*                               *)

let inc_refresh entry = 
    let open CME_Types in
    let conv_msg_type = function
        | V_MDUpdateAction_New        -> Message_types.V_MDUpdateAction_New          
        | V_MDUpdateAction_Change     -> Message_types.V_MDUpdateAction_Change       
        | V_MDUpdateAction_Delete     -> Message_types.V_MDUpdateAction_Delete       
        | V_MDUpdateAction_DeleteThru -> Message_types.V_MDUpdateAction_DeleteThru   
        | V_MDUpdateAction_DeleteFrom -> Message_types.V_MDUpdateAction_DeleteFrom   
        | V_MDUpdateAction_Overlay    -> Message_types.V_MDUpdateAction_Overlay       
    in 
    let conv_entry_type = function
        | V_MDEntryType_Bid          -> Message_types.V_MDEntryTypeBook_Bid                  
        | V_MDEntryType_Offer        -> Message_types.V_MDEntryTypeBook_Offer                
        | V_MDEntryType_ImpliedBid   -> Message_types.V_MDEntryTypeBook_ImpliedBid           
        | V_MDEntryType_ImpliedOffer -> Message_types.V_MDEntryTypeBook_ImpliedOffer         
        | V_MDEntryType_EmptyBook    -> Message_types.V_MDEntryTypeBook_BookReset            
    in
    {
        Message_types.f_MDIncrementalRefreshBook32_NoMDEntries_SecurityID     = entry.rm_security_id |> Int32.of_int;
        Message_types.f_MDIncrementalRefreshBook32_NoMDEntries_RptSeq         = entry.rm_rep_seq_num |> Int32.of_int;
        Message_types.f_MDIncrementalRefreshBook32_NoMDEntries_MDPriceLevel   = entry.rm_price_level;
        Message_types.f_MDIncrementalRefreshBook32_NoMDEntries_MDEntrySize    = entry.rm_entry_size  |> (function 0 -> None | x -> Some ( Int32.of_int x ) );
        Message_types.f_MDIncrementalRefreshBook32_NoMDEntries_MDEntryPx      = entry.rm_entry_px    |> int_to_pricenull ;
        Message_types.f_MDIncrementalRefreshBook32_NoMDEntries_NumberOfOrders = entry.rm_num_orders  |> (function None -> None | Some x -> Some ( Int32.of_int x ) );
        Message_types.f_MDIncrementalRefreshBook32_NoMDEntries_MDUpdateAction = conv_msg_type   entry.rm_msg_type  ;
        Message_types.f_MDIncrementalRefreshBook32_NoMDEntries_MDEntryType    = conv_entry_type entry.rm_entry_type
    };;

(*                               *)
(*    Snapshot part              *)
(*                               *)

let snap_refresh time msg = 
    let open Message_types in 
    let open CME_Types in
    let time = Int64.of_int time in
    let settp = { 
        r_SettlPriceType_Final        = false;
        r_SettlPriceType_Actual       = false;
        r_SettlPriceType_Rounded      = false;
        r_SettlPriceType_Intraday     = false;
        r_SettlPriceType_ReservedBits = false;
        r_SettlPriceType_NullValue    = true
    } in
    let make_entry etype level entry = {
        f_SnapshotFullRefresh38_NoMDEntries_MDEntryPx            = entry.price      |> int_to_pricenull;
        f_SnapshotFullRefresh38_NoMDEntries_MDEntrySize          = entry.qty        |> int_to_int32_opt; 
        f_SnapshotFullRefresh38_NoMDEntries_NumberOfOrders       = entry.num_orders |> int_opt_to_int32_opt;
        f_SnapshotFullRefresh38_NoMDEntries_MDPriceLevel         = level;
        f_SnapshotFullRefresh38_NoMDEntries_TradingReferenceDate = None;
        f_SnapshotFullRefresh38_NoMDEntries_OpenCloseSettlFlag   = V_OpenCloseSettlFlag_Null;
        f_SnapshotFullRefresh38_NoMDEntries_SettlPriceType       = settp;
        f_SnapshotFullRefresh38_NoMDEntries_MDEntryType          = etype
        } in
    let convert etype lst = 
        let rec scan level = function
            | None   :: tl -> scan level tl
            | Some l :: tl ->
                let l = make_entry etype (Some level) l in
                l :: scan (level + 1) tl
            | [] -> [] in
        scan 1 lst in
    let entries = 
        let snp = msg.sm_snapshot in
        List.concat [
            convert V_MDEntryType_Bid          snp.snap_m_book.buys; 
            convert V_MDEntryType_Offer        snp.snap_m_book.sells;
            convert V_MDEntryType_ImpliedBid   snp.snap_i_book.buys; 
            convert V_MDEntryType_ImpliedOffer snp.snap_i_book.sells
        ] in
    {
        f_SnapshotFullRefresh38_LastMsgSeqNumProcessed     = msg.sm_snapshot.snap_last_msg_seq_num_processed |> Int32.of_int ;
        f_SnapshotFullRefresh38_SecurityID                 = msg.sm_security_id |> Int32.of_int ;
        f_SnapshotFullRefresh38_RptSeq                     = msg.sm_rep_seq_num |> Int32.of_int ;
        f_SnapshotFullRefresh38_NoMDEntries                = entries ;
        (* NOTE -- transact time is the same as the sending time *)
        f_SnapshotFullRefresh38_TransactTime               = time ; 
        (* NOTE -- last update time is the same as the sending time *)
        f_SnapshotFullRefresh38_LastUpdateTime             = time ; 
        (* I'm just making this stuff up*)
        f_SnapshotFullRefresh38_TotNumReports              = Int32.of_int 1               ;
        f_SnapshotFullRefresh38_HighLimitPrice             = int_to_pricenull 0           ;
        f_SnapshotFullRefresh38_LowLimitPrice              = int_to_pricenull 0           ;
        f_SnapshotFullRefresh38_MaxPriceVariation          = int_to_pricenull 0           ;
        f_SnapshotFullRefresh38_TradeDate                  = None                         ; 
        f_SnapshotFullRefresh38_MDSecurityTradingStatus    = V_SecurityTradingStatus_Null
    }
;;

let matchevent = { (* NOTE -- that one is probably wrong *)
    Message_types.r_MatchEventIndicator_LastTradeMsg   = false;
    Message_types.r_MatchEventIndicator_LastVolumeMsg  = false;
    Message_types.r_MatchEventIndicator_LastQuoteMsg   = false;
    Message_types.r_MatchEventIndicator_LastStatsMsg   = false;
    Message_types.r_MatchEventIndicator_LastImpliedMsg = false;
    Message_types.r_MatchEventIndicator_RecoveryMsg    = false;
    Message_types.r_MatchEventIndicator_Reserved       = false;
    Message_types.r_MatchEventIndicator_EndOfEvent     = false
};;

let make_metadata seq_num time = {
    Binparser.seqence_num  = Int32.of_int seq_num;
    Binparser.sent_ts      = Int64.of_int time;
    (* NOTE -- receive time is the same as the sending time *)
    Binparser.receive_ts   = Int64.of_int time; 
    Binparser.template_id  = 32;
    Binparser.size         = 0;
    Binparser.msg_size     = 0;
    Binparser.block_length = 0;
    Binparser.schema_id    = 1
};;


let packet_write bits time (packet : CME_Types.packet ) = 
    let open CME_Types in
    let md = make_metadata packet.packet_seq_num time in
    let filter_ref  acc = function  RefreshMessage m -> ( inc_refresh m)::acc | _ -> acc in  
    let filter_snap acc = function SnapshotMessage m -> (snap_refresh time m)::acc | _ -> acc in  
    match packet.packet_channel with
        | Ch_Ref_A | Ch_Ref_B -> 
            let msg = Message_types.M_MDIncrementalRefreshBook32 {
                Message_types.f_MDIncrementalRefreshBook32_TransactTime = Int64.of_int time;
                Message_types.f_MDIncrementalRefreshBook32_MatchEventIndicator = matchevent;
                Message_types.f_MDIncrementalRefreshBook32_NoMDEntries = 
                    packet.packet_messages |> List.fold_left filter_ref []  
            } in Writers.write_message bits md msg
        | Ch_Snap_A | Ch_Snap_B ->
            packet.packet_messages 
                |> List.fold_left filter_snap []
                |> List.map ( fun x -> Message_types.M_SnapshotFullRefresh38 x ) 
                |> List.fold_left ( fun b -> fun m -> Writers.write_message b md m ) bits 
;;


