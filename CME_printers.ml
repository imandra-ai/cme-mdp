(**

    Aesthetic Integration Ltd.
    Copyright 2016

    CME_printers.ml

*)

(* @meta[imandra_ignore] on @end *)
open CME;;
(* @meta[imandra_ignore] off @end *)

(** We need to include the 'is_implied' here to make sure that we're printing *)
(** the correct number of characters *)
let print_empty_level (is_implied : bool) =
    if is_implied then
        Printf.sprintf "%11s%10s" "-" "-"
    else
        Printf.sprintf "%11s%10s%10s" "-" "-" "-"
;;

(**  *)
let print_nonempty_level (d, is_implied: order_info * bool) =
    if is_implied then
        Printf.sprintf "%11d%10.2f"
        d.qty
        (dec_to_float d.price)
    else
        match d.num_orders with
        | None -> ""
        | Some n_orders ->
        Printf.sprintf "%10d%11d%10.2f"
        d.qty
        n_orders
        (dec_to_float d.price)
;;

(** print_level: prints out a level of the book *)
let print_level (is_implied, x: bool * order_level) =
    match x with
    | NoLevel -> print_empty_level (is_implied)
    | Level d -> print_nonempty_level (d, is_implied)
;;

(** print_book_level: print out levels for the various books, etc... *)
let rec print_book_level (buys, sells, idx, num_levels, res_str, is_implied : order_level list * order_level list * int * int * string * bool) =
    if idx > num_levels then
        res_str
    else
        let b_str =
            match buys with
            | [] -> print_empty_level (is_implied)
            | x::xs -> print_level (is_implied, x)
        in let s_str =
            match sells with
            | [] -> print_empty_level (is_implied)
            | y::ys -> print_level(is_implied, y) in

        let lev_str = (Printf.sprintf "%s%9s%s\n" b_str "" s_str) in
        let xs = if List.length buys > 1 then List.tl buys else [] in
        let ys = if List.length sells > 1 then List.tl sells else [] in
        print_book_level (xs, ys, idx+1, num_levels, res_str ^ lev_str, is_implied)
;;

(** Prints out the book as a table *)
let book_to_string_long (book, is_implied, num_levels : book * bool * int) =
    let header_str =
        if is_implied then
            Printf.sprintf "%10s%10s%20s%10s\n"
            "Quantity" "Price" "Quantity" "Price"
        else
            Printf.sprintf "%10s%10s%10s%20s%10s%10s\n"
            "Order Count" "Quantity" "Price" "Order Count" "Quantity" "Price" in
    let levels_str = print_book_level (book.buys, book.sells, 1, num_levels, "", is_implied) in
    header_str ^ levels_str
;;

(** Used in printing out state transitions *)
let print_ord_short (o, is_implied : order_level * bool) =
    match o with
        | NoLevel -> "[ No Info ]"
        | Level d ->
            let n_orders = (
                match d.num_orders with
                    | None -> 0
                    | Some n -> n) in
            Printf.sprintf "[%.2f:%d:%d]"
                (dec_to_float d.price)
                d.qty
                n_orders
;;

let rec print_order_levels ( ord_levels, ret_str, is_implied : order_level list * string * bool) =
    match ord_levels with
    | [] -> ret_str
(*    | [x] -> print_ord_short (x, is_implied) *)
    | x::xs -> print_order_levels (xs, ret_str ^ ";\n" ^ print_ord_short(x, is_implied), is_implied)
;;

let rec print_book_nice ( buy_orders, sell_orders, ret_str, is_implied : order_level list * order_level list * string * bool) = 
    let empty_str = "[ No Info ]" in 
    
    match buy_orders, sell_orders with 
    | b::bx, s::sx -> 
        let b_str = print_ord_short (b, is_implied) in 
        let s_str = print_ord_short (s, is_implied) in 
        let curr_str = b_str ^ ":" ^ s_str ^ "\n" in
        print_book_nice (bx, sx, curr_str ^ ret_str, is_implied)
    
    | [], s::sx -> 
        let s_str = print_ord_short (s, is_implied) in 
        let curr_str = empty_str ^ ":" ^ s_str ^ "\n" in 
        print_book_nice ([], sx, curr_str ^ ret_str, is_implied)

    | b::bx, [] -> 
        let b_str = print_ord_short (b, is_implied) in 
        let curr_str = empty_str ^ ":" ^ b_str ^ "\n" in 
        print_book_nice (bx, [], curr_str ^ ret_str, is_implied)

    | _, _ -> ret_str
;;

(** Print out the book in a short format. *)
let book_to_string_short (book, is_implied, num_levels : book * bool * int) =
    let buy_str     = print_order_levels (book.buys,  "", is_implied) in
    let sell_str    = print_order_levels (book.sells, "", is_implied) in
    buy_str ^ ":" ^ sell_str
;;

let one_book_to_string (books, bt : books * book_type) =
    match bt with
    | Book_Type_Multi    -> book_to_string_long (books.multi,   false, books.book_depth)
    | Book_Type_Implied  -> book_to_string_long (books.implied,  true, books.book_depth)
    | Book_Type_Combined -> book_to_string_long (books.combined, true, books.book_depth)
;;

let books_to_string (books) =
    Printf.sprintf "\nMulti-depth book:\n%s\nImplied book:\n%s\nConsolidated book:\n%s\n"
    (book_to_string_long (books.multi,    false, books.book_depth))
    (book_to_string_long (books.implied,  true,  books.book_depth))
    (book_to_string_long (books.combined, true,  books.book_depth))
;;

let books_to_string_short (books) =
    Printf.sprintf "\nmulti:\n%s\nimplied:\n%s\ncombined:\n%s\n"
    (print_book_nice (books.multi.buys,     books.multi.sells,      "", false))
    (print_book_nice (books.implied.buys,   books.implied.sells,    "", false))
    (print_book_nice (books.combined.buys,  books.combined.sells,   "", false))
;;

let internal_type_to_string it =
    match it with
    | Book_Changed_to_InRecovery -> "Book_Changed_to_InRecovery"
    | Book_Changed_to_Normal     -> "Book_Changed_to_Normal"
    | Book_Proc_Normal_Update    -> "Book_Proc_Normal_Update"
    | Book_Proc_Cache_Add        -> "Book_Proc_Cache_Add"
(*    | Book_Proc_NotRelevant      -> "Book_Proc_NotRelevant" *)
;;


(** convert snapshot packet to string *)
let snap_msg_to_str (sm : snap_message) =
    Printf.sprintf ""
;;

let entry_type_to_str (entry_type : entry_type) =
    match entry_type with
    V_MDEntryType_Bid -> "V_MDEntryType_Bid"
  | V_MDEntryType_Offer -> "V_MDEntryType_Offer"
  | V_MDEntryType_ImpliedBid -> "V_MDEntryType_ImpliedBid"
  | V_MDEntryType_ImpliedOffer -> "V_MDEntryType_ImpliedOffer"
  | V_MDEntryType_EmptyBook -> "V_MDEntryType_EmptyBook"
  
  (**
  | V_MDEntryType_TradeSummary -> "V_MDEntryType_TradeSummary"
  | V_MDEntryType_OpeningPrice -> "V_MDEntryType_OpeningPrice"
  | V_MDEntryType_SettlementPrice -> "V_MDEntryType_SettlementPrice"
  | V_MDEntryType_TradingSessionHighPrice -> "V_MDEntryType_TradingSessionHighPrice"
  | V_MDEntryType_TradingSessionLowPrice -> "V_MDEntryType_TradingSessionLowPrice"
  | V_MDEntryType_SessionHighBid -> "V_MDEntryType_SessionHighBid"
  | V_MDEntryType_SessionLowOffer -> "V_MDEntryType_SessionLowOffer"
  | V_MDEntryType_TradeVolume -> "V_MDEntryType_TradeVolume"
  | V_MDEntryType_OpenInterest -> "V_MDEntryType_OpenInterest"
  | V_MDEntryType_FixingPrice -> "V_MDEntryType_FixingPrice"
  | V_MDEntryType_ElectronicVolume -> "V_MDEntryType_ElectronicVolume"
  | V_MDEntryType_ThresholdLimits -> "V_MDEntryType_ThresholdLimits"
    *)
;;

let msg_type_to_str (msg_type : msg_type) =
    match msg_type with
    | V_MDUpdateAction_New -> "V_MDUpdateAction_New"
    | V_MDUpdateAction_Change -> "V_MDUpdateAction_Change"
    | V_MDUpdateAction_Delete -> "V_MDUpdateAction_Delete"
    | V_MDUpdateAction_DeleteThru -> "V_MDUpdateAction_DeleteThru"
    | V_MDUpdateAction_DeleteFrom -> "V_MDUpdateAction_DeleteFrom"
    | V_MDUpdateAction_Overlay -> "V_MDUpdateAction_Overlay"
;;

let ord_level_to_str (ol : order_level) =
    match ol with
    | NoLevel -> "Empty"
    | Level d ->
        Printf.sprintf "[side=%s;price=%.2f;qty=%d;num_orders=%s]"
        (if d.side = BUY then "Buy" else "Sell")
        (dec_to_float (d.price))
        d.qty
        (match d.num_orders with
            | None -> "Unavail"
            | Some p -> Printf.sprintf "%d" p)
;;

(** convert incremental refresh packet to string *)
let ref_msg_to_str (rm : ref_message)  =
    Printf.sprintf "ref_msg[sec_id=%d, rep_seq_num=%d, msg_type=%s, entry_type=%s, price_level=%d, entry_size=%d, entry_px=%d, num_orders=%d]"
    rm.rm_security_id
    rm.rm_rep_seq_num
    (msg_type_to_str rm.rm_msg_type)
    (entry_type_to_str rm.rm_entry_type)
    rm.rm_price_level
    rm.rm_entry_size
    rm.rm_entry_px
    rm.rm_num_orders
;;

(** print out the cache *)
let rec print_cache ( c : ref_message list) = 
    match c with 
    | [] -> ""
    | x::xs -> 
        let curr_str = ref_msg_to_str (x) in 
        curr_str ^ "\n" ^ print_cache(xs)
;;

(** Print internal state transition message as string *)
let internal_msg_to_str (im_msg : internal_msg) =
    let books = im_msg.im_books in
    Printf.sprintf "\nstart_of_state_change:%s:time=%d:\n%s\ncache:\n%s\n:end_of_state_change\n"
        (internal_type_to_string im_msg.im_change_type)
        im_msg.im_time
        (books_to_string_short books)
        (print_cache (im_msg.im_cache))
;;

(** This is the simplest format for printing out internal state changes. *)
let internal_msg_to_str_simple (in_msg : internal_msg) = 
    let books = in_msg.im_books in 
    Printf.sprintf "%s"
    (books_to_string_short books)
;;


(** print out the internal messages with only the books displayed *)
let rec int_messages_to_str (msgs : internal_msg list) =
    match msgs with
    | [] -> ""
    | x::xs ->
        let curr_str = internal_msg_to_str (x) in
        curr_str ^ int_messages_to_str (xs)
;;


(** print out the internal messages with a required format *)
let rec int_messages_to_str_format (msgs : internal_msg list) =
    let order_to_str = function 
        | NoLevel -> "X X X" 
        | Level x -> 
            let num_ord = match x.num_orders with None -> 0 | Some x -> x in
            Printf.sprintf "%d %d %d" num_ord x.price x.qty
    in 
    let order_list_to_str prefix olist = 
        olist |> List.map order_to_str
              |> String.concat " "
              |> Printf.sprintf "%s %d %s" prefix  (List.length olist)
        in
    let books_to_str msgnum msg = [
            order_list_to_str "MB" msg.im_books.multi.buys     ;
            order_list_to_str "MA" msg.im_books.multi.sells    ;
            order_list_to_str "IB" msg.im_books.implied.buys   ;
            order_list_to_str "IA" msg.im_books.implied.sells  ;
            order_list_to_str "CB" msg.im_books.combined.buys  ;
            order_list_to_str "CA" msg.im_books.combined.sells 
        ] |> String.concat "\n"
          |> Printf.sprintf "N %d \n%s" msgnum
        in
    msgs |> List.mapi books_to_str
         |> String.concat "\n\n" 
;;




(** print out the interal messages in a simplified format *)
let rec int_messages_to_str_simple (msgs : internal_msg list) = 
    match msgs with 
    | [] -> ""
    | x::xs -> 
        let curr_str = internal_msg_to_str_simple (x) in 
        curr_str ^ int_messages_to_str_simple (xs)
;;

(** print_state_messages *)
let rec print_state_messages ( msgs : internal_msg list ) =
        match msgs with
        | [] -> ""
        | x::xs -> let m = internal_msg_to_str(x) in
                                m ^ "\n" ^ print_state_messages (xs)
;;

(** print_packet_header *)
let print_packet_header (ph : packet_header) =
    Printf.sprintf "[packet_seq_num=%d;sending_time=%d]"
    ph.ph_packet_seq_num
    ph.ph_sending_time
;;

(** ref_packet_to_str *)
let rec ref_packet_to_str (packets : ref_packet list) =
    match packets with
    | [] -> ""
    | x::xs ->
    let curr_str = (
        let header_str = print_packet_header (x.rp_header) in
        let msg_str = ref_msg_to_str (x.rp_msg) in
        Printf.sprintf "begin_packet:\n%s\n%s\n:end_packet\n"
        header_str
        msg_str
    ) in
    curr_str ^ ref_packet_to_str (xs)
;;

(** Convert a list of packets to a string *)
let rec ref_packet_list_to_str (packets : ref_packet_lst list) =
    match packets with
    | [] -> ""
    | x::xs ->
    let curr_str = (
        let header_str = print_packet_header (x.rpl_header) in
        let msgs_str = String.concat "\n" (List.map ref_msg_to_str x.rpl_msgs) in
        Printf.sprintf "begin_packet:\n%s\n%s\n:end_packet\n"
            header_str
            msgs_str
    ) in
    curr_str ^ ref_packet_list_to_str (xs)
;;

let snap_msg_to_str (sm : snap_message) =
    Printf.sprintf "snap_msg[sec_id=%d, last_msg_seq_num=%d, rep_seq_num=%d, real_bid=%s, real_ask=%s, implied_bid=%s, implied_ask=%s]"
    sm.sm_security_id
    sm.sm_last_msg_seq_num_processed
    sm.sm_rep_seq_num
    (ord_level_to_str (sm.sm_real_bid))
    (ord_level_to_str (sm.sm_real_ask))
    (ord_level_to_str (sm.sm_imp_bid))
    (ord_level_to_str (sm.sm_imp_ask))
;;

let rec snap_packet_to_str (packets : snap_packet list) =
    match packets with
    | [] -> ""
    | x::xs ->
    let curr_str = (
        let header_str = print_packet_header (x.sp_header) in
        let msg_str = snap_msg_to_str (x.sp_snap) in
        Printf.sprintf "begin_packet:\n%s\n%s\n:end_packet\n"
            header_str
            msg_str
    ) in
    curr_str ^ snap_packet_to_str (xs)
;;

let rec snap_packet_list_to_str (packets : snap_packet_lst list) =
    match packets with
    | [] -> ""
    | x::xs ->
    let curr_str = (
        let header_str = print_packet_header (x.spl_header) in
        let msgs_str = String.concat "\n" (List.map snap_msg_to_str x.spl_snap) in
        Printf.sprintf "begin_packet:\n%s\n%s\n:end_packet\n"
            header_str
            msgs_str
    ) in
    curr_str ^ snap_packet_list_to_str (xs)
;;
