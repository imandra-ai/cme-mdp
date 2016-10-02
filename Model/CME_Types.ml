(**
    Aesthetic Integration Ltd.
    Copyright 2016

    CME_Types.ml
*)

(** *************************************************************** *)
(** Define the types that we support                                *)
(** *************************************************************** *)
type sec_type = SecA | SecB ;;

(** *************************************************************** *)
(** Orders and books                                                *)
(** *************************************************************** *)
type order_side = OrdBuy | OrdSell;;

(** Order_info is used to represent levels within order book *)
type order_info = {
    side       : order_side;
    qty        : int;
    price      : int;
    num_orders : int option; (* Not provided for implied books *)
};;

type order_level = Level of order_info | NoLevel;;

(** Book sorting information                                       *)
let order_higher_ranked (s, o1, o2 : order_side * order_level * order_level) =
    match o1, o2 with
    | Level d_1, Level d_2 -> if s = OrdBuy then d_1.price > d_2.price else d_1.price < d_2.price
    | Level d_1, NoLevel -> true
    | NoLevel, Level d_2 -> false
    | NoLevel, NoLevel -> true
;;

(** Generic book type (used for all three types of books) *)
type book = {
    buys : order_level list;
    sells : order_level list;
};;

(** *************************************************************** *)
(** Message types                                                   *)
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
    | V_MDEntryType_Bid
    | V_MDEntryType_Offer
    | V_MDEntryType_ImpliedBid
    | V_MDEntryType_ImpliedOffer
    | V_MDEntryType_EmptyBook
;;

type ref_message = {
    rm_security_id : int;
    rm_rep_seq_num : int;
    rm_msg_type    : msg_type;

    rm_entry_type  : entry_type; (* Bid, etc. *)
    rm_price_level : int;
    rm_entry_size  : int;
    rm_entry_px    : int;
    rm_num_orders  : int option;
};;


(* Snapshot message *)
type snapshot = {
    snap_m_book : book; (* "multi-depth" book *)
    snap_i_book : book; (* implied book       *)
    snap_last_msg_seq_num_processed : int (* this corresponds to packet number for Incremental update *)
};;

type snap_message = {
    sm_security_id : int;
    sm_rep_seq_num : int;                   (* this corresponds to instrument RepSeqNum *)
    sm_snapshot: snapshot;
};;

type message =
    | RefreshMessage  of ref_message
    | SnapshotMessage of snap_message
;;

type channel_type =
    | Ch_Ref_A
    | Ch_Ref_B
    | Ch_Snap_A
    | Ch_Snap_B
;;

type packet = {
    packet_seq_num  : int;
    packet_messages : message list;
    packet_channel  : channel_type
};;

