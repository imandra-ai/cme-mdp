
#use "topfind";;
#require "yojson";;

:load Model/CME_Types.ml
:load Model/CME_Exchange.ml
:load Model/CME_Network.ml
:load_ocaml Printers/CME_json.ml
:load_ocaml Printers/CME_Exchange_json.ml

type action =
    | BookAction     of book_transition
    | ExchangeAction of exchange_transition
;;


(* A recursive run function.
   Note how this implicitly includes transition validity.

   @meta[measure : run]
     let measure_run (s, actions) = List.length actions
   @end
*)
let rec run (state, acts) =
    match state, acts with
    |    _ , [] -> state
    | None ,  _ -> state
    | Some s, BookAction act :: acts -> 
        let es = process_book_trans (s, act) in 
        run (Some es, acts)  
    | Some s, ExchangeAction act :: acts -> 
        let es = process_exchange_trans (s, act) in 
        run (Some es, acts)
;;

(* We set up run for staged symbolic execution *)
:stage run

(* @meta[measure : valid]
    let measure_valid (s, actions) = List.length actions
    @end
*)

let rec valid (s, acts) =
    match (s, acts) with 
    | None   , _  -> false 
    | Some _ , [] -> true
    | Some s, BookAction act :: acts ->  
        is_book_trans_valid (s, act) && (
        let es = process_book_trans (s, act) in 
        valid (Some es, acts) )
    | Some s, ExchangeAction act :: acts -> 
        is_exchange_trans_valid (s, act) && (
        let es = process_exchange_trans (s, act) in 
        valid (Some es, acts) )
;;

let make_ord_change_data (x1,x2,x3,x4,x5) = {
    oc_sec_type = x1;
    oc_book_type = x2;
    oc_level_num = x3;
    oc_level_side = x4;
    oc_new_qty = x5;
    };;
let make_ord_del_data (x1,x2,x3,x4) = {
    od_sec_type = x1;
    od_book_type = x2;
    od_level_side = x3;
    od_level_num = x4;
    };;
let make_ord_add_data (x1,x2,x3,x4,x5,x6,x7) = {
    oa_order_qty = x1;
    oa_sec_type = x2;
    oa_book_type = x3;
    oa_level_num = x4;
    oa_level_side = x5;
    oa_num_orders = x6;
    oa_price = x7;
    };;

type search_space = {
    oa_price_1:int;
    oa_price_2:int;
    oa_price_3:int;
    oa_price_4:int;
    oa_price_5:int;
    oa_price_6:int;
    oa_price_7:int;
    oa_price_8:int;
    oa_price_9:int;
    oa_price_10:int;
    oa_price_11:int;
    oa_price_12:int;
    oa_price_13:int;
    oa_price_14:int;
    oa_price_15:int;
    oa_price_16:int;
    oa_price_17:int;
    oa_price_18:int;
    oa_price_19:int;
    oa_price_20:int;
    oa_price_21:int;
    oa_price_22:int;
    oa_price_23:int;
    oa_price_24:int;
    oa_price_25:int;
    oa_price_26:int;
    oa_price_27:int;
    oa_price_28:int;
    oa_price_29:int;
    oa_price_30:int;
    oa_price_31:int;
    oa_price_32:int;
    oa_price_33:int;
    oa_price_34:int;
    oa_price_35:int;
    oa_price_36:int;
    oa_price_37:int;
    oa_price_38:int;
    oa_price_39:int;
    oa_price_40:int;
    od_level_num_87:int;
    oc_new_qty_59:int;
    oc_new_qty_76:int;
    od_level_num_82:int;
    oc_new_qty_65:int;
    od_level_num_83:int;
    oc_new_qty_43:int;
    oc_new_qty_66:int;
    };;


let search_space_to_list x = [
    BookAction(ST_Add(make_ord_add_data(1,SecA,Book_Type_Implied,1,OrdBuy,Some 1,x.oa_price_1)));
    BookAction(ST_Add(make_ord_add_data(1,SecA,Book_Type_Implied,1,OrdSell,Some 1,x.oa_price_2)));
    BookAction(ST_Add(make_ord_add_data(1,SecA,Book_Type_Implied,2,OrdBuy,Some 1,x.oa_price_3)));
    BookAction(ST_Add(make_ord_add_data(1,SecA,Book_Type_Implied,2,OrdSell,Some 1,x.oa_price_4)));
    BookAction(ST_Add(make_ord_add_data(1,SecA,Book_Type_Implied,3,OrdBuy,Some 1,x.oa_price_5)));
    BookAction(ST_Add(make_ord_add_data(1,SecA,Book_Type_Implied,3,OrdSell,Some 1,x.oa_price_6)));
    BookAction(ST_Add(make_ord_add_data(1,SecA,Book_Type_Implied,4,OrdBuy,Some 1,x.oa_price_7)));
    BookAction(ST_Add(make_ord_add_data(1,SecA,Book_Type_Implied,4,OrdSell,Some 1,x.oa_price_8)));
    BookAction(ST_Add(make_ord_add_data(1,SecA,Book_Type_Implied,5,OrdBuy,Some 1,x.oa_price_9)));
    BookAction(ST_Add(make_ord_add_data(1,SecA,Book_Type_Implied,5,OrdSell,Some 1,x.oa_price_10)));
    BookAction(ST_Add(make_ord_add_data(1,SecA,Book_Type_Multi,1,OrdBuy,Some 1,x.oa_price_11)));
    BookAction(ST_Add(make_ord_add_data(1,SecA,Book_Type_Multi,1,OrdSell,Some 1,x.oa_price_12)));
    BookAction(ST_Add(make_ord_add_data(1,SecA,Book_Type_Multi,2,OrdBuy,Some 1,x.oa_price_13)));
    BookAction(ST_Add(make_ord_add_data(1,SecA,Book_Type_Multi,2,OrdSell,Some 1,x.oa_price_14)));
    BookAction(ST_Add(make_ord_add_data(1,SecA,Book_Type_Multi,3,OrdBuy,Some 1,x.oa_price_15)));
    BookAction(ST_Add(make_ord_add_data(1,SecA,Book_Type_Multi,3,OrdSell,Some 1,x.oa_price_16)));
    BookAction(ST_Add(make_ord_add_data(1,SecA,Book_Type_Multi,4,OrdBuy,Some 1,x.oa_price_17)));
    BookAction(ST_Add(make_ord_add_data(1,SecA,Book_Type_Multi,4,OrdSell,Some 1,x.oa_price_18)));
    BookAction(ST_Add(make_ord_add_data(1,SecA,Book_Type_Multi,5,OrdBuy,Some 1,x.oa_price_19)));
    BookAction(ST_Add(make_ord_add_data(1,SecA,Book_Type_Multi,5,OrdSell,Some 1,x.oa_price_20)));
    BookAction(ST_Add(make_ord_add_data(1,SecB,Book_Type_Implied,1,OrdBuy,Some 1,x.oa_price_21)));
    BookAction(ST_Add(make_ord_add_data(1,SecB,Book_Type_Implied,1,OrdSell,Some 1,x.oa_price_22)));
    BookAction(ST_Add(make_ord_add_data(1,SecB,Book_Type_Implied,2,OrdBuy,Some 1,x.oa_price_23)));
    BookAction(ST_Add(make_ord_add_data(1,SecB,Book_Type_Implied,2,OrdSell,Some 1,x.oa_price_24)));
    BookAction(ST_Add(make_ord_add_data(1,SecB,Book_Type_Implied,3,OrdBuy,Some 1,x.oa_price_25)));
    BookAction(ST_Add(make_ord_add_data(1,SecB,Book_Type_Implied,3,OrdSell,Some 1,x.oa_price_26)));
    BookAction(ST_Add(make_ord_add_data(1,SecB,Book_Type_Implied,4,OrdBuy,Some 1,x.oa_price_27)));
    BookAction(ST_Add(make_ord_add_data(1,SecB,Book_Type_Implied,4,OrdSell,Some 1,x.oa_price_28)));
    BookAction(ST_Add(make_ord_add_data(1,SecB,Book_Type_Implied,5,OrdBuy,Some 1,x.oa_price_29)));
    BookAction(ST_Add(make_ord_add_data(1,SecB,Book_Type_Implied,5,OrdSell,Some 1,x.oa_price_30)));
    BookAction(ST_Add(make_ord_add_data(1,SecB,Book_Type_Multi,1,OrdBuy,Some 1,x.oa_price_31)));
    BookAction(ST_Add(make_ord_add_data(1,SecB,Book_Type_Multi,1,OrdSell,Some 1,x.oa_price_32)));
    BookAction(ST_Add(make_ord_add_data(1,SecB,Book_Type_Multi,2,OrdBuy,Some 1,x.oa_price_33)));
    BookAction(ST_Add(make_ord_add_data(1,SecB,Book_Type_Multi,2,OrdSell,Some 1,x.oa_price_34)));
    BookAction(ST_Add(make_ord_add_data(1,SecB,Book_Type_Multi,3,OrdBuy,Some 1,x.oa_price_35)));
    BookAction(ST_Add(make_ord_add_data(1,SecB,Book_Type_Multi,3,OrdSell,Some 1,x.oa_price_36)));
    BookAction(ST_Add(make_ord_add_data(1,SecB,Book_Type_Multi,4,OrdBuy,Some 1,x.oa_price_37)));
    BookAction(ST_Add(make_ord_add_data(1,SecB,Book_Type_Multi,4,OrdSell,Some 1,x.oa_price_38)));
    BookAction(ST_Add(make_ord_add_data(1,SecB,Book_Type_Multi,5,OrdBuy,Some 1,x.oa_price_39)));
    BookAction(ST_Add(make_ord_add_data(1,SecB,Book_Type_Multi,5,OrdSell,Some 1,x.oa_price_40)));
    ExchangeAction(ST_DataSendInc);
    ExchangeAction(ST_Snapshot(SecA));
    ExchangeAction(ST_DataSendSnap);
    ExchangeAction(ST_Snapshot(SecB));
    ExchangeAction(ST_DataSendSnap);
    BookAction(ST_Delete(make_ord_del_data(SecB,Book_Type_Multi,OrdBuy,x.od_level_num_87)));
    BookAction(ST_Change(make_ord_change_data(SecA,Book_Type_Multi,5,OrdBuy,x.oc_new_qty_59)));
    BookAction(ST_Change(make_ord_change_data(SecB,Book_Type_Multi,3,OrdSell,x.oc_new_qty_76)));
    BookAction(ST_Delete(make_ord_del_data(SecA,Book_Type_Implied,OrdSell,x.od_level_num_82)));
    BookAction(ST_Change(make_ord_change_data(SecB,Book_Type_Implied,3,OrdBuy,x.oc_new_qty_65)));
    BookAction(ST_Delete(make_ord_del_data(SecA,Book_Type_Multi,OrdBuy,x.od_level_num_83)));
    BookAction(ST_Change(make_ord_change_data(SecA,Book_Type_Implied,2,OrdBuy,x.oc_new_qty_43)));
    BookAction(ST_Change(make_ord_change_data(SecB,Book_Type_Implied,3,OrdSell,x.oc_new_qty_66)));
    ExchangeAction(ST_DataSendInc);
    ExchangeAction(ST_Snapshot(SecA));
    ExchangeAction(ST_DataSendSnap);
    ExchangeAction(ST_Snapshot(SecB));
    ExchangeAction(ST_DataSendSnap);
    ];;


 
let run_all m = run ( Some init_ex_state, search_space_to_list m ) ;;

let valid_all m = valid ( Some init_ex_state, search_space_to_list m ) ;;

:shadow off
let n = ref 0;;
let write_jsons m =
    let final_state = run ( Some init_ex_state, search_space_to_list m ) in
    match final_state with 
    | None -> " **** Ignoring empty test case ***** " |> print_string
    | Some final_state ->
    let () = n := !n + 1 in
    final_state |> exchange_state_to_json
                |> Yojson.Basic.to_file (Printf.sprintf "generatedStates/test_ef143cc1c6499cb2f925587ba57414fc_%d.json" !n) 
;;
:shadow on
:adts on

:max_region_time 120
:testgen run_all assuming valid_all with_code write_jsons 
