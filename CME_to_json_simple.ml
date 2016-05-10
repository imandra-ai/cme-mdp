(**

    Aesthetic Integration Ltd.

    CME_to_json_simple.ml

*)

(* @meta[imandra_ignore] on @end *)
open CME;;
(* @meta[imandra_ignore] off @end *)

let string_of_agg_side x =
    match x with
      NoAggressor -> "NoAggressor"
    | BuyAggressor -> "BuyAggressor"
    | SellAggressor -> "SellAggressor"
;;

let string_of_msg_type x =
    match x with
    | V_MDUpdateAction_New -> "New"
    | V_MDUpdateAction_Change -> "Change"
    | V_MDUpdateAction_Delete -> "Delete"
    | V_MDUpdateAction_DeleteThru -> "DeleteThru"
    | V_MDUpdateAction_DeleteFrom -> "DeleteFrom"
    | V_MDUpdateAction_Overlay -> "Overlay"
;;

let string_of_entry_type x =
    match x with
    V_MDEntryType_Bid -> "Bid"
  | V_MDEntryType_Offer -> "Offer"
  | V_MDEntryType_ImpliedBid -> "ImpliedBid"
  | V_MDEntryType_ImpliedOffer -> "ImpliedOffer"
  | V_MDEntryType_EmptyBook -> "EmptyBook"

(** 
  | V_MDEntryType_TradeSummary -> "TradeSummary"
  | V_MDEntryType_OpeningPrice -> "OpeningPrice"
  | V_MDEntryType_SettlementPrice -> "SettlementPrice"
  | V_MDEntryType_TradingSessionHighPrice -> "TradingSessionHighPrice"
  | V_MDEntryType_TradingSessionLowPrice -> "TradingSessionLowPrice"
  | V_MDEntryType_SessionHighBid -> "SessionHighBid"
  | V_MDEntryType_SessionLowOffer -> "SessionLowOffer"
  | V_MDEntryType_TradeVolume -> "TradeVolume"
  | V_MDEntryType_OpenInterest -> "OpenInterest"
  | V_MDEntryType_FixingPrice -> "FixingPrice"
  | V_MDEntryType_ElectronicVolume -> "ElectronicVolume"
  | V_MDEntryType_ThresholdLimits -> "ThresholdLimits"
*)
;;

let string_of_side x = 
  match x with 
  | BUY -> "BUY"
  | SELL -> "SELL"
;;

let string_of_num_orders n = 
  match n with 
  | None -> "{}"
  | Some k -> 
    Printf.sprintf "%d" k
;;

let string_of_price p = 
  Printf.sprintf "%.4f" 
    (dec_to_float (p))
;;

let string_of_level l =
  match l with 
  | NoLevel -> "\n"
  | Level k ->
    Printf.sprintf "
    \"Side\" : \"%s\",
    \"Quantity\" : %d,
    \"Price\" : %s,
    \"NumOrders\" : %s
    "
    (string_of_side k.side)
    k.qty
    (string_of_price k.price)
    (string_of_num_orders k.num_orders)
;;

let string_of_packet p =
    match p with
    | NoPacket -> ""
    | SnapshotPacket p ->
        Printf.sprintf
        "{
        \"SnapshotPacket\" : {
        \"Header\" : {
          \"SequenceNumber\": %d,
          \"SendingTime\": %d
        },
        \"Message\" : {
          \"SecurityID\" : %d,
          \"RepSeqNum\" : %d,
          \"LastMsgSeqNumProcessed\" : %d,
          \"RealBid\" : %s,
          \"RealAsk\" : %s,
          \"ImplBid\" : %s,
          \"ImplAsk\" : %s
        }
        }
        }\n"
      p.sp_header.ph_packet_seq_num
      p.sp_header.ph_sending_time
      p.sp_snap.sm_security_id
      p.sp_snap.sm_rep_seq_num
      p.sp_snap.sm_last_msg_seq_num_processed
      (string_of_level p.sp_snap.sm_real_bid)
      (string_of_level p.sp_snap.sm_real_ask)
      (string_of_level p.sp_snap.sm_imp_bid) 
      (string_of_level p.sp_snap.sm_imp_ask)

    | IncRefreshPacket p ->
        Printf.sprintf
      "{
        \"IncRefreshPacket\" : {
          \"Header\" : {
            \"SequenceNumber\": %d,
            \"SendingTime\": %d
          },
        \"Message\" : {
          \"SequrityID\": %d,
          \"RepSeqNum\": %d,
          \"PriceLevel\": %d,
          \"EntrySize\": %d,
          \"EntryPrice\": %d,
          \"NumOrders\": %d,
          \"MessageType\": \"%s\",
          \"EntryType\": \"%s\"
        }
        }
      }
      \n"
      p.rp_header.ph_packet_seq_num
      p.rp_header.ph_sending_time
      p.rp_msg.rm_security_id
      p.rp_msg.rm_rep_seq_num
      p.rp_msg.rm_price_level
      p.rp_msg.rm_entry_size
      p.rp_msg.rm_entry_px
      p.rp_msg.rm_num_orders
      (string_of_msg_type p.rp_msg.rm_msg_type)
      (string_of_entry_type p.rp_msg.rm_entry_type)
;;

let string_of_packets packets =
  Printf.sprintf
  "
  {
    \"packets\" : [
      %s
    ]
  }
  "
  (String.concat ",\n " (List.map string_of_packet packets))
;;


