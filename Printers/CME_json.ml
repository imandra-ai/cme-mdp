(**    

    Aesthetic Integration Ltd.
    Copyright 2016
    
    CME_printers.ml

*)

(* @meta[imandra_ignore] on @end *)
open CME;;
(* @meta[imandra_ignore] off @end *)

let packet_to_json (packet : packet) : Kojson.json  =
    let open Kojson in
    let convert_header h = Assoc [
        ("SequenceNumber", Int h.ph_packet_seq_num);
        ("SendingTime",    Int h.ph_sending_time)
        ] in
    let convert_inc  p = Assoc [
        ( "SecurityID", Int p.rm_security_id );
        ( "RepSeqNum",  Int p.rm_rep_seq_num );
        ( "PriceLevel", Int p.rm_price_level );
        ( "EntrySize",  Int p.rm_entry_size  );
        ( "EntryPrice", Int p.rm_entry_px    );
        ( "NumOrders",  Int p.rm_num_orders  );
        ( "MessageType",   match p.rm_msg_type with
            | V_MDUpdateAction_New         -> String "New"       
            | V_MDUpdateAction_Change      -> String "Change"    
            | V_MDUpdateAction_Delete      -> String "Delete"    
            | V_MDUpdateAction_DeleteThru  -> String "DeleteThru"
            | V_MDUpdateAction_DeleteFrom  -> String "DeleteFrom"
            | V_MDUpdateAction_Overlay     -> String "Overlay"    
            );
        ( "EntryType", match p.rm_entry_type with
            | V_MDEntryType_Bid                      -> String "Bid"                    
            | V_MDEntryType_Offer                    -> String "Offer"                  
            | V_MDEntryType_ImpliedBid               -> String "ImpliedBid"             
            | V_MDEntryType_ImpliedOffer             -> String "ImpliedOffer"  
            | V_MDEntryType_EmptyBook                -> String "EmptyBook"              
         
          (*
            | V_MDEntryType_TradeSummary             -> String "TradeSummary"           
            | V_MDEntryType_OpeningPrice             -> String "OpeningPrice"           
            | V_MDEntryType_SettlementPrice          -> String "SettlementPrice"        
            | V_MDEntryType_TradingSessionHighPrice  -> String "TradingSessionHighPrice"
            | V_MDEntryType_TradingSessionLowPrice   -> String "TradingSessionLowPrice" 
            | V_MDEntryType_SessionHighBid           -> String "SessionHighBid"         
            | V_MDEntryType_SessionLowOffer          -> String "SessionLowOffer"        
            | V_MDEntryType_TradeVolume              -> String "TradeVolume"            
            | V_MDEntryType_OpenInterest             -> String "OpenInterest"           
            | V_MDEntryType_FixingPrice              -> String "FixingPrice"            
            | V_MDEntryType_ElectronicVolume         -> String "ElectronicVolume"       
            | V_MDEntryType_ThresholdLimits          -> String "ThresholdLimits"   *) 
            ) 
        ] in
    let convert_order_level = function
        | NoLevel -> Assoc []
        | Level o -> Assoc [
            ("Side",      match o.side with BUY -> String "BUY" | SELL -> String "SELL");
            ("Quantity", Int o.qty   );
            ("Price",    Int o.price  );
            ("NumOrders", match o.num_orders with None -> Assoc [] | Some n -> Int n )
        ] in 
    let convert_snap p = Assoc[
            ("SecurityID", Int p.sm_security_id );
            ("RepSeqNum",  Int p.sm_rep_seq_num );
            ("LastMsgSeqNumProcessed",  Int p.sm_last_msg_seq_num_processed );
            ("RealBid", convert_order_level p.sm_real_bid );
            ("RealAsk", convert_order_level p.sm_real_ask );
            ("ImplBid", convert_order_level p.sm_imp_bid  );
            ("ImplAsk", convert_order_level p.sm_imp_ask  )
        ] in
    match packet with
        | SnapshotPacket   p -> Assoc [ ( "SnapshotPacket" ,   
            Assoc [ ( "Header" , convert_header p.sp_header  );
                     ( "Message", convert_snap p.sp_snap ) ] 
            ) ]
        | IncRefreshPacket p -> Assoc [ ( "IncRefreshPacket" , 
            Assoc [ ( "Header" , convert_header p.rp_header  );
                     ( "Message", convert_inc p.rp_msg ) ] 
            ) ];;
        

let packets_to_json (packets : packet list) : Kojson.json  =
    let open Kojson in 
    Assoc [( "packets", List ( List.map packet_to_json packets ))] ;;

let packets_to_string (packets : packet list) : string  = 
    packets |> packets_to_json |> Kojson.to_string;;

(*    
let packet_of_json ( json : Kojson.json) : packet   =
    let open Kojson in
    let fail msg = raise (Failure ("Malformed packet JSON: " ^ msg )) in
    let getint = function Int n -> n | _ -> fail "integer is expected" in
    let read_header = function 
        | Assoc lst -> {
                ph_packet_seq_num = lst |> List.assoc "SequenceNumber" |> getint;
                ph_sending_time   = lst |> List.assoc "SendingTime"    |> getint 
            } 
        | _ -> fail "malformed header"
        in
    let read_inc = function
        | Assoc lst -> {
                rm_security_id = lst |> List.assoc "SequrityID" |> getint;
                rm_rep_seq_num = lst |> List.assoc "RepSeqNum"  |> getint;
                rm_price_level = lst |> List.assoc "PriceLevel" |> getint;
                rm_entry_size  = lst |> List.assoc "EntrySize"  |> getint;
                rm_entry_px    = lst |> List.assoc "EntryPrice" |> getint;
                rm_num_orders  = lst |> List.assoc "NumOrders"  |> getint;
                rm_msg_type    = lst 
                    |> List.assoc "MessageType" 
                    |> ( function 
                          | String "New"        ->  V_MDUpdateAction_New         
                          | String "Change"     ->  V_MDUpdateAction_Change      
                          | String "Delete"     ->  V_MDUpdateAction_Delete      
                          | String "DeleteThru" ->  V_MDUpdateAction_DeleteThru  
                          | String "DeleteFrom" ->  V_MDUpdateAction_DeleteFrom  
                          | String "Overlay"    ->  V_MDUpdateAction_Overlay     
                          | _ -> fail "bad message type" );
                rm_entry_type = lst
                    |> List.assoc "EntryType"
                    |> ( function 
                         | String "Bid"                     -> V_MDEntryType_Bid                    
                         | String "Offer"                   -> V_MDEntryType_Offer                  
                         | String "ImpliedBid"              -> V_MDEntryType_ImpliedBid             
                         | String "ImpliedOffer"            -> V_MDEntryType_ImpliedOffer
                         | String "EmptyBook"               -> V_MDEntryType_EmptyBook              
                      (*
                         | String "TradeSummary"            -> V_MDEntryType_TradeSummary           
                         | String "OpeningPrice"            -> V_MDEntryType_OpeningPrice           
                         | String "SettlementPrice"         -> V_MDEntryType_SettlementPrice        
                         | String "TradingSessionHighPrice" -> V_MDEntryType_TradingSessionHighPrice
                         | String "TradingSessionLowPrice"  -> V_MDEntryType_TradingSessionLowPrice 
                         | String "SessionHighBid"          -> V_MDEntryType_SessionHighBid         
                         | String "SessionLowOffer"         -> V_MDEntryType_SessionLowOffer        
                         | String "TradeVolume"             -> V_MDEntryType_TradeVolume            
                         | String "OpenInterest"            -> V_MDEntryType_OpenInterest           
                         | String "FixingPrice"             -> V_MDEntryType_FixingPrice            
                         | String "EmptyBook"               -> V_MDEntryType_EmptyBook              
                         | String "ElectronicVolume"        -> V_MDEntryType_ElectronicVolume       
                         | String "ThresholdLimits"         -> V_MDEntryType_ThresholdLimits     *)   
                         | _ -> fail "bad entry type")
            }
        | _ -> fail "malformed incref packet"
        in
    let read_order_level = function
        | Assoc []  -> NoLevel  
        | Assoc lst -> Level {
                qty   = lst |> List.assoc "Quantity" |> getint;
                price = lst |> List.assoc "Price"    |> getint;
                side  = lst |> List.assoc "Side" 
                                |> (function | String "BUY"  -> BUY 
                                             | String "SELL" -> SELL
                                             | _ -> fail "side should be either BUY or SELL");
                num_orders = lst |> List.assoc "NumOrders" 
                                     |> (function | Assoc [] -> None
                                                  | Int n -> Some n
                                                  | _ -> fail "Bad NUmOrders" )
            }
        | _ -> fail "bad order level"
       in 
    let read_snap  = function
        | Assoc lst -> {
                sm_security_id = lst |> List.assoc "SecurityID" |> getint ;
                sm_rep_seq_num = lst |> List.assoc "RepSeqNum"  |> getint ;
                sm_last_msg_seq_num_processed = lst |> List.assoc "LastMsgSeqNumProcessed" |> getint;
                sm_real_bid    = lst |> List.assoc "RealBid" |> read_order_level;
                sm_real_ask    = lst |> List.assoc "RealAsk" |> read_order_level;
                sm_imp_bid     = lst |> List.assoc "ImplBid" |> read_order_level;
                sm_imp_ask     = lst |> List.assoc "ImplAsk" |> read_order_level
            }
        | _ -> fail "malformed snap packet"
        in
    match json with
        | Assoc [ ( "SnapshotPacket" ,   Assoc lst ) ] ->
            let h, m = List.assoc "Header" lst, List.assoc "Message" lst in
            SnapshotPacket   { sp_header = read_header h; sp_snap = read_snap m }
        | Assoc [ ( "IncRefreshPacket" , Assoc lst ) ] ->
            let h, m = List.assoc "Header" lst, List.assoc "Message" lst in
            IncRefreshPacket { rp_header = read_header h; rp_msg  = read_inc m }
        

let packets_of_json (json : Kojson.json) : packet list =
    let open Kojson in 
    match json with
        | Assoc [( "packets", List packets)] -> List.map packet_of_json packets
        | _ -> raise (Failure "A single toplevel \"packets\" key is expected.")
*)
