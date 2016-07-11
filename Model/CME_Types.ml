(**
    Aesthetic Integration Ltd.
    Copyright 2016

    CME_Exchange.ml
*)

type order_side = OrdBuy | OrdSell;;

(** Order_info is used to represent levels within order book *)
type order_info = {
    side       : order_side;
    qty        : int;
    price      : int;
    num_orders : int option; (* Not provided for implied books *)
};;

(** Order_level *)
type order_level = NoLevel | Level of order_info;;

(** *************************************************************** *)
(** Define the types that we support                                *)
(** *************************************************************** *)
type sec_type = SecA | SecB ;;
type book_type = 
    | Book_Type_Implied 
    | Book_Type_Multi 
    | Book_Type_Combined 
;;
(** *************************************************************** *)

(** The various types *)
type msg_type =
    | V_MDUpdateAction_New
    | V_MDUpdateAction_Change
    | V_MDUpdateAction_Delete
    | V_MDUpdateAction_DeleteThru
    | V_MDUpdateAction_DeleteFrom
    | V_MDUpdateAction_Overlay
;;

type entry_type =
    V_MDEntryType_Bid
  | V_MDEntryType_Offer
  | V_MDEntryType_ImpliedBid
  | V_MDEntryType_ImpliedOffer
  | V_MDEntryType_EmptyBook
;;

(** *************************************************************** *)
(** Msg *)
(** *************************************************************** *)

type ref_message = {
    rm_security_id : int;
    rm_rep_seq_num : int;
    rm_msg_type    : msg_type;

    rm_entry_type  : entry_type; (* Bid, etc. *)
    rm_price_level : int;
    rm_entry_size  : int;
    rm_entry_px    : int;
    rm_num_orders  : int;
};;

(** Internal snapshot message representation.                           *)
(*  Note that in 'real' format, this would be spread across numerous    *)
(*  messages within the same (potentially different) packets.           *)
type snap_message = {
    sm_security_id : int;
    sm_last_msg_seq_num_processed : int;    (* this corresponds to packet number for Incremental update *)
    sm_rep_seq_num : int;               (* this corresponds to instrument RepSeqNum *)

    sm_real_bid : order_level;
    sm_real_ask : order_level;
    sm_imp_bid  : order_level;
    sm_imp_ask  : order_level;
} ;;

(** *************************************************************** *)

(** *************************************************************** *)
(* This is the top-level packet type including the global sequence  *)
(* number                                                           *)
(** *************************************************************** *)

(** Note about connecting Snapshots and Incremental Refresh messages:
        --> Market recovery packet message contains field 369-LastMsgSeqNumProcessed
            it corresponds to Incremental Refresh Message
        --> Tag 83 RptSeq
*)
type packet_header = {
    ph_packet_seq_num : int;
    (** Corresponds to MsgSeqNum
        Packet sequence number.
        A unique sequence number given to each packet sent.
        Each channel will have its own separate set of sequence numbers that will increment sequentially
        with each packet and reset weekly.

        See: http://www.cmegroup.com/confluence/display/EPICSANDBOX/MDP+3.0+-+Binary+Packet+Header
     *)
    ph_sending_time : int;
    (** Corresponds to SendingTime
        UTC Time of message transmission by the Gateway. UTC Timestamps are sent in number of nanoseconds
        since Unix epoch with guaranteed microsecond precision.
    *)
};;

(** Note about packet data types:
    In the actual binary (and other) format, a packet has a header and a *list* of messages. For the purposes
    of modelling the feed, we do not need to introduce this complexity. In the model, we use ref_packet and
    snap_packet types. Note the at we have no restriction on repeating packet sequence numbers, hence a natural
    way to replicate a *real* packet with multiple messages is just by having a list of *model* packets with
    the same packet header, but different message fields.

    Types ref_packet_lst & snap_packet_lst are to be used by the encoder/decoder to read/write from/into
    the binary format.
*)
type ref_packet  = { rp_header : packet_header; rp_msg  : ref_message;  };;
type snap_packet = { sp_header : packet_header; sp_snap : snap_message; };;

(** We have two types of packets here: for Market Recovery (Snapshot) and Incremental
    Refresh *)
type packet = SnapshotPacket of snap_packet | IncRefreshPacket of ref_packet | NoPacket;;


