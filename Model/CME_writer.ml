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
    let conv_msg_type = function
        | CME.Action_New        -> Message_types.V_MDUpdateAction_New          
        | CME.Action_Change     -> Message_types.V_MDUpdateAction_Change       
        | CME.Action_Delete     -> Message_types.V_MDUpdateAction_Delete       
        | CME.Action_DeleteThru -> Message_types.V_MDUpdateAction_DeleteThru   
        | CME.Action_DeleteFrom -> Message_types.V_MDUpdateAction_DeleteFrom   
        | CME.Action_Overlay    -> Message_types.V_MDUpdateAction_Overlay       
    in 
    let conv_entry_type = function
        | CME.Entry_Bid          -> Message_types.V_MDEntryTypeBook_Bid                  
        | CME.Entry_Offer        -> Message_types.V_MDEntryTypeBook_Offer                
        | CME.Entry_ImpliedBid   -> Message_types.V_MDEntryTypeBook_ImpliedBid           
        | CME.Entry_ImpliedOffer -> Message_types.V_MDEntryTypeBook_ImpliedOffer         
        | CME.Entry_EmptyBook    -> Message_types.V_MDEntryTypeBook_BookReset            
    in
    {
        Message_types.f_MDIncrementalRefreshBook32_NoMDEntries_SecurityID     = entry.CME.rm_security_id |> Int32.of_int;
        Message_types.f_MDIncrementalRefreshBook32_NoMDEntries_RptSeq         = entry.CME.rm_rep_seq_num |> Int32.of_int;
        Message_types.f_MDIncrementalRefreshBook32_NoMDEntries_MDPriceLevel   = entry.CME.rm_price_level;
        Message_types.f_MDIncrementalRefreshBook32_NoMDEntries_MDEntrySize    = entry.CME.rm_entry_size  |> (function 0 -> None | x -> Some ( Int32.of_int x ) );
        Message_types.f_MDIncrementalRefreshBook32_NoMDEntries_MDEntryPx      = entry.CME.rm_entry_px    |> int_to_pricenull ;
        Message_types.f_MDIncrementalRefreshBook32_NoMDEntries_NumberOfOrders = entry.CME.rm_num_orders  |> (function 0 -> None | x -> Some ( Int32.of_int x ) );
        Message_types.f_MDIncrementalRefreshBook32_NoMDEntries_MDUpdateAction = conv_msg_type   entry.CME.rm_msg_type  ;
        Message_types.f_MDIncrementalRefreshBook32_NoMDEntries_MDEntryType    = conv_entry_type entry.CME.rm_entry_type
    };;

let ref_packet_write channel (packets : CME.ref_packet list) = 
    (* let packets = CME_test_helper.merge_ref_packets ( packets , [] ) in *)
    let me = { (* NOTE -- that one is probably wrong *)
        Message_types.r_MatchEventIndicator_LastTradeMsg   = false;
        Message_types.r_MatchEventIndicator_LastVolumeMsg  = false;
        Message_types.r_MatchEventIndicator_LastQuoteMsg   = false;
        Message_types.r_MatchEventIndicator_LastStatsMsg   = false;
        Message_types.r_MatchEventIndicator_LastImpliedMsg = false;
        Message_types.r_MatchEventIndicator_RecoveryMsg    = false;
        Message_types.r_MatchEventIndicator_Reserved       = false;
        Message_types.r_MatchEventIndicator_EndOfEvent     = false
    } in
    let write bits p =
        let open CME in
        let md = {
            Binparser.seqence_num  = Int32.of_int p.rp_header.ph_packet_seq_num;
            Binparser.sent_ts      = Int64.of_int p.rp_header.ph_sending_time;
            (* NOTE -- receive time is the same as the sending time *)
            Binparser.receive_ts   = Int64.of_int p.rp_header.ph_sending_time; 
            Binparser.template_id  = 32;
            Binparser.size         = 0;
            Binparser.msg_size     = 0;
            Binparser.block_length = 0;
            Binparser.schema_id    = 1
        } in
        let msg = Message_types.M_MDIncrementalRefreshBook32 {
            Message_types.f_MDIncrementalRefreshBook32_TransactTime = Int64.of_int p.rp_header.ph_sending_time;
            Message_types.f_MDIncrementalRefreshBook32_MatchEventIndicator = me;
            Message_types.f_MDIncrementalRefreshBook32_NoMDEntries = [ inc_refresh p.rp_msg ]
        } in
        Writers.write_message bits md msg
    in
    List.fold_left write channel packets
;;


(*                               *)
(*    Snapshot part              *)
(*                               *)

let snap_refresh time msg = 
    let open Message_types in 
    let settp = { 
        r_SettlPriceType_Final        = false;
        r_SettlPriceType_Actual       = false;
        r_SettlPriceType_Rounded      = false;
        r_SettlPriceType_Intraday     = false;
        r_SettlPriceType_ReservedBits = false;
        r_SettlPriceType_NullValue    = true
    } in
    let make_entry etype level entry = {
        f_SnapshotFullRefresh38_NoMDEntries_MDEntryPx      = entry.CME.price      |> int_to_pricenull;
        f_SnapshotFullRefresh38_NoMDEntries_MDEntrySize    = entry.CME.qty        |> int_to_int32_opt; 
        f_SnapshotFullRefresh38_NoMDEntries_NumberOfOrders = entry.CME.num_orders |> int_opt_to_int32_opt;
        f_SnapshotFullRefresh38_NoMDEntries_MDPriceLevel         = level;
        f_SnapshotFullRefresh38_NoMDEntries_TradingReferenceDate = None;
        f_SnapshotFullRefresh38_NoMDEntries_OpenCloseSettlFlag   = V_OpenCloseSettlFlag_Null;
        f_SnapshotFullRefresh38_NoMDEntries_SettlPriceType       = settp;
        f_SnapshotFullRefresh38_NoMDEntries_MDEntryType          = etype
        } in
    let convert etype lst = 
        let rec scan level = function
            | CME.NoLevel :: tl -> scan level tl
            | CME.Level l :: tl ->
                let l = make_entry etype (Some level) l in
                l :: scan (level + 1) tl
            | [] -> [] in
        scan 1 lst in
    let entries = 
        let snp = msg.CME.sm_snapshot in
        List.concat [
            convert V_MDEntryType_Bid          snp.CME.snap_m_book.CME.buys; 
            convert V_MDEntryType_Offer        snp.CME.snap_m_book.CME.sells;
            convert V_MDEntryType_ImpliedBid   snp.CME.snap_i_book.CME.buys; 
            convert V_MDEntryType_ImpliedOffer snp.CME.snap_i_book.CME.sells
        ] in
    {
        f_SnapshotFullRefresh38_LastMsgSeqNumProcessed     = msg.CME.sm_last_msg_seq_num_processed |> Int32.of_int ;
        f_SnapshotFullRefresh38_SecurityID                 = msg.CME.sm_security_id |> Int32.of_int ;
        f_SnapshotFullRefresh38_RptSeq                     = msg.CME.sm_rep_seq_num |> Int32.of_int ;
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


let snap_packet_write channel (packets : CME.snap_packet list) = 
    let write bits p =
        let open CME in
        let time = Int64.of_int p.sp_header.ph_sending_time in
        let md = {
            Binparser.seqence_num  = Int32.of_int p.sp_header.ph_packet_seq_num;
            Binparser.sent_ts      = time;
            Binparser.receive_ts   = time; (* NOTE -- receive time is the same as the sending time *)
            Binparser.template_id  = 38;
            Binparser.size         = 0;
            Binparser.msg_size     = 0;
            Binparser.block_length = 0;
            Binparser.schema_id    = 1
        } in
        let msg = Message_types.M_SnapshotFullRefresh38 (snap_refresh time p.sp_snap) in
        Writers.write_message bits md msg
    in
    List.fold_left write channel packets



