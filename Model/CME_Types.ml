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

(** *************************************************************** *)
(** Define the types that we support                                *)
(** *************************************************************** *)
type sec_type = 
      FUTURES 
    | SPREAD 
    | OPTION 
;;
type book_type = 
    | Book_Type_Implied 
    | Book_Type_Multi 
    | Book_Type_Combined 
;;
(** *************************************************************** *)

