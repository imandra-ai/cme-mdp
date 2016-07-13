(**
    Aesthetic Integration Ltd.
    Copyright 2016

    CME_Types.ml
*)

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

type order_level = order_info option ;;

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

(* entry_type utility function *)
let side_to_entry_type ( book_type, side : book_type * order_side) = 
    match ( book_type, side ) with 
    | ( Book_Type_Implied  , OrdBuy  ) -> V_MDEntryType_ImpliedBid
    | ( Book_Type_Multi    , OrdBuy  ) -> V_MDEntryType_Bid
    | ( Book_Type_Combined , OrdBuy  ) -> V_MDEntryType_Bid
    | ( Book_Type_Implied  , OrdSell ) -> V_MDEntryType_ImpliedOffer
    | ( Book_Type_Multi    , OrdSell ) -> V_MDEntryType_Offer
    | ( Book_Type_Combined , OrdSell ) -> V_MDEntryType_Offer
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

type snap_message = {
    sm_security_id : int;
    sm_last_msg_seq_num_processed : int;    (* this corresponds to packet number for Incremental update *)
    sm_rep_seq_num : int;                   (* this corresponds to instrument RepSeqNum *)

    sm_real_bid : order_level;
    sm_real_ask : order_level;
    sm_imp_bid  : order_level;
    sm_imp_ask  : order_level;
} ;;


type message =
    | RefreshMessage  of ref_message
    | SnapshotMessage of snap_message
;;

type packet = {
    packet_seq_num  : int;
    packet_messages : message list
};;

