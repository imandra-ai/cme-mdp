#use "topfind";;
#require "yojson";;

:load Model/CME_Types.ml
:load Model/CME_Exchange.ml
:load Model/CME_Network.ml
:load_ocaml Printers/CME_json.ml

type state = {
    exchange_state : exchange_state ;
     network_state :  network_state 
};;

type action =
    | BookAction     of book_transition
    | ExchangeAction of exchange_transition
    | CopyPackets
    | NetworkAction  of net_effect
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
        let es = process_book_trans (s.exchange_state, act) in 
        run (Some {s with exchange_state = es}, acts)  
    | Some s, ExchangeAction act :: acts -> 
        let es = process_exchange_trans (s.exchange_state, act) in 
        run (Some {s with exchange_state = es}, acts)
    | Some s, NetworkAction  act :: acts -> 
        let ns = process_net_effect (s.network_state, act) in 
        run (Some { s with network_state = ns } , acts ) 
    | Some s, CopyPackets :: acts ->
        let s = Some { s with network_state = 
            { s.network_state with 
                incoming = s.exchange_state.pac_queue
            }
        } in run ( s , acts)
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
        is_book_trans_valid (s.exchange_state, act) && (
        let es = process_book_trans (s.exchange_state, act) in 
        valid (Some {s with exchange_state = es}, acts) )
    | Some s, ExchangeAction act :: acts -> 
        is_exchange_trans_valid (s.exchange_state, act) && (
        let es = process_exchange_trans (s.exchange_state, act) in 
        valid (Some {s with exchange_state = es}, acts) )
    | Some s,  NetworkAction  act :: acts -> 
        is_neteffect_valid (s.network_state, act) && (
        let ns = process_net_effect (s.network_state, act) in 
        valid (Some {s with network_state = ns}, acts) )
    | Some s, CopyPackets :: acts ->
        let s = Some { s with network_state = 
            { s.network_state with 
                incoming = s.exchange_state.pac_queue
            }
        } in valid ( s , acts)
;;


let mk_add_data (px,lvl,side,book,sec) = {
    oa_order_qty  = 1;
    oa_price      = px;
    oa_sec_type   = sec;
    oa_book_type  = book;
    oa_level_num  = lvl;
    oa_level_side = side;
    oa_num_orders = Some 1
};;

let preparation = [ 
    (BookAction(ST_Add((mk_add_data (90,1,OrdBuy,Book_Type_Multi,SecA)))));
    (BookAction(ST_Add((mk_add_data (110,1,OrdSell,Book_Type_Multi,SecA)))));
    (BookAction(ST_Add((mk_add_data (80,2,OrdBuy,Book_Type_Multi,SecA)))));
    (BookAction(ST_Add((mk_add_data (120,2,OrdSell,Book_Type_Multi,SecA)))));
    (BookAction(ST_Add((mk_add_data (70,3,OrdBuy,Book_Type_Multi,SecA)))));
    (BookAction(ST_Add((mk_add_data (130,3,OrdSell,Book_Type_Multi,SecA)))));
    (BookAction(ST_Add((mk_add_data (60,4,OrdBuy,Book_Type_Multi,SecA)))));
    (BookAction(ST_Add((mk_add_data (140,4,OrdSell,Book_Type_Multi,SecA)))));
    (BookAction(ST_Add((mk_add_data (50,5,OrdBuy,Book_Type_Multi,SecA)))));
    (BookAction(ST_Add((mk_add_data (150,5,OrdSell,Book_Type_Multi,SecA)))));
    (BookAction(ST_Add((mk_add_data (90,1,OrdBuy,Book_Type_Multi,SecB)))));
    (BookAction(ST_Add((mk_add_data (110,1,OrdSell,Book_Type_Multi,SecB)))));
    (BookAction(ST_Add((mk_add_data (80,2,OrdBuy,Book_Type_Multi,SecB)))));
    (BookAction(ST_Add((mk_add_data (120,2,OrdSell,Book_Type_Multi,SecB)))));
    (BookAction(ST_Add((mk_add_data (70,3,OrdBuy,Book_Type_Multi,SecB)))));
    (BookAction(ST_Add((mk_add_data (130,3,OrdSell,Book_Type_Multi,SecB)))));
    (BookAction(ST_Add((mk_add_data (60,4,OrdBuy,Book_Type_Multi,SecB)))));
    (BookAction(ST_Add((mk_add_data (140,4,OrdSell,Book_Type_Multi,SecB)))));
    (BookAction(ST_Add((mk_add_data (50,5,OrdBuy,Book_Type_Multi,SecB)))));
    (BookAction(ST_Add((mk_add_data (150,5,OrdSell,Book_Type_Multi,SecB)))));
];;

type search_space = { 
    
     x1 : ord_change_data; 
     x2 : ord_change_data; 
     x3 : ord_change_data; 
    
};;

let search_space_to_list x = [
    (ExchangeAction(ST_DataSendInc ));
    (BookAction(ST_Change x.x1));
    (BookAction(ST_Change x.x2));
    (BookAction(ST_Change x.x3));
    (CopyPackets );
];;

let empty_state = Some {
    exchange_state = init_ex_state;
    network_state = empty_network_state  
};;

let prepared_state = run (empty_state, preparation);;

let run_all m = run ( prepared_state, search_space_to_list m ) ;;

let valid_all m = valid ( prepared_state, search_space_to_list m ) ;;

:shadow off
let n = ref 0;;
let write_jsons m =
    let final_state = run ( empty_state, search_space_to_list m ) in
    match final_state with 
    | None -> " **** Ignoring empty test case ***** " |> print_string
    | Some final_state ->
    let packets = final_state.network_state.outgoing @ final_state.network_state.incoming in
    let () = n := !n + 1 in
    packets |> packets_to_json
            (*|> Yojson.Basic.pretty_to_string |> print_string *)
            |> Yojson.Basic.to_file (Printf.sprintf "generated/test_%d.json" !n) 
;;
:shadow on
:adts on

:max_region_time 120
:testgen run_all assuming valid_all with_code write_jsons 


