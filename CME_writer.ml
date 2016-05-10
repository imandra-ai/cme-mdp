let int_to_pricenull i =
    let m = match i with 
        | 0 -> None 
        | x -> Some (Int64.of_int (x * ( 10000000 / CME.dec_units ) ) ) 
    in { Message_types.f_PRICENULL_mantissa =  m;
         Message_types.f_PRICENULL_exponent = -7; }


(*                               *)
(*    Incremental refresh part   *)
(*                               *)

let inc_refresh entry = 
    let conv_msg_type = function
        | CME.V_MDUpdateAction_New        -> Message_types.V_MDUpdateAction_New          
        | CME.V_MDUpdateAction_Change     -> Message_types.V_MDUpdateAction_Change       
        | CME.V_MDUpdateAction_Delete     -> Message_types.V_MDUpdateAction_Delete       
        | CME.V_MDUpdateAction_DeleteThru -> Message_types.V_MDUpdateAction_DeleteThru   
        | CME.V_MDUpdateAction_DeleteFrom -> Message_types.V_MDUpdateAction_DeleteFrom   
        | CME.V_MDUpdateAction_Overlay    -> Message_types.V_MDUpdateAction_Overlay       
    in 
    let conv_entry_type = function
        | CME.V_MDEntryType_Bid                     -> Message_types.V_MDEntryTypeBook_Bid                  
        | CME.V_MDEntryType_Offer                   -> Message_types.V_MDEntryTypeBook_Offer                
        | CME.V_MDEntryType_ImpliedBid              -> Message_types.V_MDEntryTypeBook_ImpliedBid           
        | CME.V_MDEntryType_ImpliedOffer            -> Message_types.V_MDEntryTypeBook_ImpliedOffer         
        | CME.V_MDEntryType_EmptyBook               -> Message_types.V_MDEntryTypeBook_BookReset        
        | CME.V_MDEntryType_TradeSummary            -> Message_types.V_MDEntryTypeBook_TradeSummary
        | CME.V_MDEntryType_OpeningPrice            -> Message_types.V_MDEntryTypeBook_OpeningPrice
        | CME.V_MDEntryType_SettlementPrice         -> Message_types.V_MDEntryTypeBook_SettlementPrice
        | CME.V_MDEntryType_TradingSessionHighPrice -> Message_types.V_MDEntryTypeBook_TradingSessionHighPrice
        | CME.V_MDEntryType_TradingSessionLowPrice  -> Message_types.V_MDEntryTypeBook_TradingSessionLowPrice
        | CME.V_MDEntryType_SessionHighBid          -> Message_types.V_MDEntryTypeBook_SessionHighBid
        | CME.V_MDEntryType_SessionLowOffer         -> Message_types.V_MDEntryType_SessionLowOffer
        | CME.V_MDEntryType_TradeVolume             -> Message_types.V_MDEntryType_TradeVolume
        | CME.V_MDEntryType_OpenInterest            -> Message_types.V_MDEntryType_OpenInterest
        | CME.V_MDEntryType_FixingPrice             -> Message_types.V_MDEntryType_FixingPrice
        | CME.V_MDEntryType_ElectronicVolume        -> Message_types.V_MDEntryType_ElectronicVolume
        | CME.V_MDEntryType_ThresholdLimits         -> Message_types.V_MDEntryType_ThresholdLimits
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

let rec ref_packet_write channel (packets : CME.ref_packet list) = 
    let packets = CME_test_helper.merge_ref_packets ( packets , [] ) in
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
            Binparser.seqence_num  = Int32.of_int p.rpl_header.ph_packet_seq_num;
            Binparser.sent_ts      = Int64.of_int p.rpl_header.ph_sending_time;
            Binparser.receive_ts   = Int64.of_int p.rpl_header.ph_sending_time; (* NOTE -- receive time is the same as the sending time *)
            Binparser.template_id  = 32;
            Binparser.size         = 0;
            Binparser.msg_size     = 0;
            Binparser.block_length = 0;
            Binparser.schema_id    = 1
        } in
        let msg = Message_types.M_MDIncrementalRefreshBook32 {
            Message_types.f_MDIncrementalRefreshBook32_TransactTime = Int64.of_int p.rpl_header.ph_sending_time;
            Message_types.f_MDIncrementalRefreshBook32_MatchEventIndicator = me;
            Message_types.f_NoMDEntries = List.map inc_refresh p.rpl_msgs
        } in
        Writers.write_message bits md msg
    in
    List.fold_left write channel packets
;;


(*                               *)
(*    Snapshot part              *)
(*                               *)

let snap_refresh msg = 
    let open Message_types in 
    let settp = { 
        r_SettlPriceType_Final        = false;
        r_SettlPriceType_Actual       = false;
        r_SettlPriceType_Rounded      = false;
        r_SettlPriceType_Intraday     = false;
        r_SettlPriceType_ReservedBits = false;
        r_SettlPriceType_NullValue    = true
    } in
    let entries = [ 
        ( V_MDEntryType_Bid          , msg.CME.sm_real_bid ) ; 
        ( V_MDEntryType_Offer        , msg.CME.sm_real_ask ) ; 
        ( V_MDEntryType_ImpliedBid   , msg.CME.sm_imp_bid  ) ; 
        ( V_MDEntryType_ImpliedOffer , msg.CME.sm_imp_ask  ) 
    ] |> List.filter ( fun ( _ , l ) -> l <> CME.NoLevel ) 
      |> List.map ( fun ( t , l ) -> (t, match l with CME.Level x -> x | CME.NoLevel -> raise (Failure "Shouldnt happen") ) )
      |> List.map ( fun ( t , l ) -> {
                            f_SnapshotFullRefresh38_NoMDEntries_MDEntryPx      = l.CME.price |> int_to_pricenull;
                            f_SnapshotFullRefresh38_NoMDEntries_MDEntrySize    = l.CME.qty |> (function 0 -> None | x -> Some ( Int32.of_int x ) ); 
                            f_SnapshotFullRefresh38_NoMDEntries_NumberOfOrders = l.CME.num_orders |> (function None -> None | Some x -> Some ( Int32.of_int x ) );
                            f_SnapshotFullRefresh38_NoMDEntries_MDPriceLevel         = None;
                            f_SnapshotFullRefresh38_NoMDEntries_TradingReferenceDate = None;
                            f_SnapshotFullRefresh38_NoMDEntries_OpenCloseSettlFlag   = V_OpenCloseSettlFlag_Null;
                            f_SnapshotFullRefresh38_NoMDEntries_SettlPriceType       = settp;
                            f_SnapshotFullRefresh38_NoMDEntries_MDEntryType          = t
                    } ) 
    in 
    {
        f_SnapshotFullRefresh38_LastMsgSeqNumProcessed     = msg.CME.sm_last_msg_seq_num_processed |> Int32.of_int ;
        f_SnapshotFullRefresh38_SecurityID                 = msg.CME.sm_security_id |> Int32.of_int ;
        f_SnapshotFullRefresh38_RptSeq                     = msg.CME.sm_rep_seq_num |> Int32.of_int ;
        f_NoMDEntries                                      = entries            ;
        (* I'm just making this stuff up*)
        f_SnapshotFullRefresh38_TransactTime               = Int64.of_int 1               ;
        f_SnapshotFullRefresh38_LastUpdateTime             = Int64.of_int 1               ;
        f_SnapshotFullRefresh38_TotNumReports              = Int32.of_int 1               ;
        f_SnapshotFullRefresh38_HighLimitPrice             = int_to_pricenull 0           ;
        f_SnapshotFullRefresh38_LowLimitPrice              = int_to_pricenull 0           ;
        f_SnapshotFullRefresh38_MaxPriceVariation          = int_to_pricenull 0           ;
        f_SnapshotFullRefresh38_TradeDate                  = None                         ; 
        f_SnapshotFullRefresh38_MDSecurityTradingStatus    = V_SecurityTradingStatus_Null
    }


let rec snap_packet_write channel (packets : CME.snap_packet list) = 
    let write bits p =
        let open CME in
        let md = {
            Binparser.seqence_num  = Int32.of_int p.sp_header.ph_packet_seq_num;
            Binparser.sent_ts      = Int64.of_int p.sp_header.ph_sending_time;
            Binparser.receive_ts   = Int64.of_int p.sp_header.ph_sending_time; (* NOTE -- receive time is the same as the sending time *)
            Binparser.template_id  = 38;
            Binparser.size         = 0;
            Binparser.msg_size     = 0;
            Binparser.block_length = 0;
            Binparser.schema_id    = 1
        } in
        let msg = Message_types.M_SnapshotFullRefresh38 (snap_refresh p.sp_snap) in
        Writers.write_message bits md msg
    in
    List.fold_left write channel packets
;;