(**    

    Aesthetic Integration Ltd.
    Copyright 2016
    
    CME_printers.ml

*)


let packet_to_json (packet : CME.packet) : Yojson.Safe.json  =
    let open Yojson.Safe in
    let convert_header h = `Assoc [
        ("SequenceNumber", `Int h.CME.ph_packet_seq_num);
        ("SendingTime",    `Int h.CME.ph_sending_time)
        ] in
    let convert_inc  p = `Assoc [
        ( "SequrityID", `Int p.CME.rm_security_id );
        ( "RepSeqNum",  `Int p.CME.rm_rep_seq_num );
        ( "PriceLevel", `Int p.CME.rm_price_level );
        ( "EntrySize",  `Int p.CME.rm_entry_size  );
        ( "EntryPrice", `Int p.CME.rm_entry_px    );
        ( "NumOrders",  `Int p.CME.rm_num_orders  );
        ( "MessageType",   match p.CME.rm_msg_type with
            | CME.V_MDUpdateAction_New         -> `String "New"       
            | CME.V_MDUpdateAction_Change      -> `String "Change"    
            | CME.V_MDUpdateAction_Delete      -> `String "Delete"    
            | CME.V_MDUpdateAction_DeleteThru  -> `String "DeleteThru"
            | CME.V_MDUpdateAction_DeleteFrom  -> `String "DeleteFrom"
            | CME.V_MDUpdateAction_Overlay     -> `String "Overlay"    
            );
        ( "EntryType", match p.CME.rm_entry_type with
            | CME.V_MDEntryType_Bid                      -> `String "Bid"                    
            | CME.V_MDEntryType_Offer                    -> `String "Offer"                  
            | CME.V_MDEntryType_ImpliedBid               -> `String "ImpliedBid"             
            | CME.V_MDEntryType_ImpliedOffer             -> `String "ImpliedOffer"  
            | CME.V_MDEntryType_EmptyBook                -> `String "EmptyBook"              
         
          (*
            | CME.V_MDEntryType_TradeSummary             -> `String "TradeSummary"           
            | CME.V_MDEntryType_OpeningPrice             -> `String "OpeningPrice"           
            | CME.V_MDEntryType_SettlementPrice          -> `String "SettlementPrice"        
            | CME.V_MDEntryType_TradingSessionHighPrice  -> `String "TradingSessionHighPrice"
            | CME.V_MDEntryType_TradingSessionLowPrice   -> `String "TradingSessionLowPrice" 
            | CME.V_MDEntryType_SessionHighBid           -> `String "SessionHighBid"         
            | CME.V_MDEntryType_SessionLowOffer          -> `String "SessionLowOffer"        
            | CME.V_MDEntryType_TradeVolume              -> `String "TradeVolume"            
            | CME.V_MDEntryType_OpenInterest             -> `String "OpenInterest"           
            | CME.V_MDEntryType_FixingPrice              -> `String "FixingPrice"            
            | CME.V_MDEntryType_ElectronicVolume         -> `String "ElectronicVolume"       
            | CME.V_MDEntryType_ThresholdLimits          -> `String "ThresholdLimits"   *) 
            ) 
        ] in
    let convert_order_level = function
        | CME.NoLevel -> `Assoc []
        | CME.Level o -> `Assoc [
            ("Side",      match o.CME.side with CME.BUY -> `String "BUY" | CME.SELL -> `String "SELL");
            ("Quantity", `Int o.CME.qty   );
            ("Price",    `Int o.CME.price  );
            ("NumOrders", match o.CME.num_orders with None -> `Assoc [] | Some n -> `Int n )
        ] in 
    let convert_snap p = `Assoc[
            ("SecurityID", `Int p.CME.sm_security_id );
            ("RepSeqNum",  `Int p.CME.sm_rep_seq_num );
            ("LastMsgSeqNumProcessed",  `Int p.CME.sm_last_msg_seq_num_processed );
            ("RealBid", convert_order_level p.CME.sm_real_bid );
            ("RealAsk", convert_order_level p.CME.sm_real_ask );
            ("ImplBid", convert_order_level p.CME.sm_imp_bid  );
            ("ImplAsk", convert_order_level p.CME.sm_imp_ask  )
        ] in
    match packet with
        | SnapshotPacket   p -> `Assoc [ ( "SnapshotPacket" ,   
            `Assoc [ ( "Header" , convert_header p.CME.sp_header  );
                     ( "Message", convert_snap p.CME.sp_snap ) ] 
            ) ]
        | IncRefreshPacket p -> `Assoc [ ( "IncRefreshPacket" , 
            `Assoc [ ( "Header" , convert_header p.CME.rp_header  );
                     ( "Message", convert_inc p.CME.rp_msg ) ] 
            ) ]
        

let packets_to_json (packets : CME.packet list) : Yojson.Safe.json  =
    let open Yojson.Safe in 
    `Assoc [( "packets", `List ( List.map packet_to_json packets ))] 


let packet_of_json ( json : Yojson.Safe.json) : CME.packet   =
    let open Yojson.Safe in
    let fail msg = raise (Failure ("Malformed packet JSON: " ^ msg )) in
    let getint = function `Int n -> n | _ -> fail "integer is expected" in
    let read_header = function 
        | `Assoc lst -> {
                CME.ph_packet_seq_num = lst |> List.assoc "SequenceNumber" |> getint;
                CME.ph_sending_time   = lst |> List.assoc "SendingTime"    |> getint 
            } 
        | _ -> fail "malformed header"
        in
    let read_inc = function
        | `Assoc lst -> {
                CME.rm_security_id = lst |> List.assoc "SequrityID" |> getint;
                CME.rm_rep_seq_num = lst |> List.assoc "RepSeqNum"  |> getint;
                CME.rm_price_level = lst |> List.assoc "PriceLevel" |> getint;
                CME.rm_entry_size  = lst |> List.assoc "EntrySize"  |> getint;
                CME.rm_entry_px    = lst |> List.assoc "EntryPrice" |> getint;
                CME.rm_num_orders  = lst |> List.assoc "NumOrders"  |> getint;
                CME.rm_msg_type    = lst 
                    |> List.assoc "MessageType" 
                    |> ( function 
                          | `String "New"        ->  CME.V_MDUpdateAction_New         
                          | `String "Change"     ->  CME.V_MDUpdateAction_Change      
                          | `String "Delete"     ->  CME.V_MDUpdateAction_Delete      
                          | `String "DeleteThru" ->  CME.V_MDUpdateAction_DeleteThru  
                          | `String "DeleteFrom" ->  CME.V_MDUpdateAction_DeleteFrom  
                          | `String "Overlay"    ->  CME.V_MDUpdateAction_Overlay     
                          | _ -> fail "bad message type" );
                CME.rm_entry_type = lst
                    |> List.assoc "EntryType"
                    |> ( function 
                         | `String "Bid"                     -> CME.V_MDEntryType_Bid                    
                         | `String "Offer"                   -> CME.V_MDEntryType_Offer                  
                         | `String "ImpliedBid"              -> CME.V_MDEntryType_ImpliedBid             
                         | `String "ImpliedOffer"            -> CME.V_MDEntryType_ImpliedOffer
                         | `String "EmptyBook"               -> CME.V_MDEntryType_EmptyBook              
                      (*
                         | `String "TradeSummary"            -> CME.V_MDEntryType_TradeSummary           
                         | `String "OpeningPrice"            -> CME.V_MDEntryType_OpeningPrice           
                         | `String "SettlementPrice"         -> CME.V_MDEntryType_SettlementPrice        
                         | `String "TradingSessionHighPrice" -> CME.V_MDEntryType_TradingSessionHighPrice
                         | `String "TradingSessionLowPrice"  -> CME.V_MDEntryType_TradingSessionLowPrice 
                         | `String "SessionHighBid"          -> CME.V_MDEntryType_SessionHighBid         
                         | `String "SessionLowOffer"         -> CME.V_MDEntryType_SessionLowOffer        
                         | `String "TradeVolume"             -> CME.V_MDEntryType_TradeVolume            
                         | `String "OpenInterest"            -> CME.V_MDEntryType_OpenInterest           
                         | `String "FixingPrice"             -> CME.V_MDEntryType_FixingPrice            
                         | `String "EmptyBook"               -> CME.V_MDEntryType_EmptyBook              
                         | `String "ElectronicVolume"        -> CME.V_MDEntryType_ElectronicVolume       
                         | `String "ThresholdLimits"         -> CME.V_MDEntryType_ThresholdLimits     *)   
                         | _ -> fail "bad entry type")
            }
        | _ -> fail "malformed incref packet"
        in
    let read_order_level = function
        | `Assoc []  -> CME.NoLevel  
        | `Assoc lst -> CME.Level {
                CME.qty   = lst |> List.assoc "Quantity" |> getint;
                CME.price = lst |> List.assoc "Price"    |> getint;
                CME.side  = lst |> List.assoc "Side" 
                                |> (function | `String "BUY"  -> CME.BUY 
                                             | `String "SELL" -> CME.SELL
                                             | _ -> fail "side should be either BUY or SELL");
                CME.num_orders = lst |> List.assoc "NumOrders" 
                                     |> (function | `Assoc [] -> None
                                                  | `Int n -> Some n
                                                  | _ -> fail "Bad NUmOrders" )
            }
        | _ -> fail "bad order level"
       in 
    let read_snap  = function
        | `Assoc lst -> {
                CME.sm_security_id = lst |> List.assoc "SecurityID" |> getint ;
                CME.sm_rep_seq_num = lst |> List.assoc "RepSeqNum"  |> getint ;
                CME.sm_last_msg_seq_num_processed = lst |> List.assoc "LastMsgSeqNumProcessed" |> getint;
                CME.sm_real_bid    = lst |> List.assoc "RealBid" |> read_order_level;
                CME.sm_real_ask    = lst |> List.assoc "RealAsk" |> read_order_level;
                CME.sm_imp_bid     = lst |> List.assoc "ImplBid" |> read_order_level;
                CME.sm_imp_ask     = lst |> List.assoc "ImplAsk" |> read_order_level
            }
        | _ -> fail "malformed snap packet"
        in
    match json with
        | `Assoc [ ( "SnapshotPacket" ,   `Assoc lst ) ] ->
            let h, m = List.assoc "Header" lst, List.assoc "Message" lst in
            SnapshotPacket   { CME.sp_header = read_header h; CME.sp_snap = read_snap m }
        | `Assoc [ ( "IncRefreshPacket" , `Assoc lst ) ] ->
            let h, m = List.assoc "Header" lst, List.assoc "Message" lst in
            IncRefreshPacket { CME.rp_header = read_header h; CME.rp_msg  = read_inc m }
        

let packets_of_json (json : Yojson.Safe.json) : CME.packet list =
    let open Yojson.Safe in 
    match json with
        | `Assoc [( "packets", `List packets)] -> List.map packet_of_json packets
        | _ -> raise (Failure "A single toplevel \"packets\" key is expected.")




