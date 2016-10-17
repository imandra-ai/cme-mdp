:load Model/CME_Testgen_Template.ml

let make_ord_change_data (x1,x2,x3,x4,x5) = {
    oc_sec_type = x1;
    oc_book_type = x2;
    oc_level_side = x3;
    oc_level_num = x4;
    oc_new_qty = x5;
    };;
let make_ord_add_data (x1,x2,x3,x4,x5,x6,x7) = {
    oa_sec_type = x1;
    oa_order_qty = x2;
    oa_level_side = x3;
    oa_level_num = x4;
    oa_num_orders = x5;
    oa_book_type = x6;
    oa_price = x7;
    };;


type search_space = {
    m1 : int;
    m2 : int;
    m3 : int;
    m4 : int;
    m5 : int;
    m6 : int;
    m7 : int;
    m8 : int;
    m9 : int;
    m10 : int;
    m11 : int;
    m12 : int;
    m13 : int;
    m14 : int;
    m15 : int;
    m16 : int;
    m17 : int;
    m18 : int;
    m19 : int;
    m20 : int;
    m21 : int;
    m22 : int;
    m23 : int;
    m24 : int;
    m25 : int;
    m26 : int;
    m27 : int;
    m28 : int;
    m29 : int;
    m30 : int;
    m31 : int;
    m32 : int;
    m33 : int;
    m34 : int;
    m35 : int;
    m36 : int;
    m37 : int;
    m38 : int;
    m39 : int;
    m40 : int;
    m41 : int;
    m42 : int;
    m43 : int;
    m44 : int;
    m45 : int;
    m46 : int;
    m47 : int;
    m48 : int;
    m49 : int;
    m50 : int;
    m51 : int;
    m52 : int;
    m53 : int;
    m54 : int;
    m55 : int;
    m56 : int;
    m57 : int;
    m58 : int;
    m59 : int;
    m60 : int;
};;

let search_space_to_list x = [
    BookAction(ST_Add(make_ord_add_data(SecA,1,OrdBuy,1,Some 1,Book_Type_Implied,x.m1)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecA,1,OrdBuy,1,Some 1,Book_Type_Multi,x.m2)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecA,1,OrdBuy,2,Some 1,Book_Type_Implied,x.m3)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecA,1,OrdBuy,2,Some 1,Book_Type_Multi,x.m4)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecA,1,OrdBuy,3,Some 1,Book_Type_Implied,x.m5)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecA,1,OrdBuy,3,Some 1,Book_Type_Multi,x.m6)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecA,1,OrdBuy,4,Some 1,Book_Type_Implied,x.m7)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecA,1,OrdBuy,4,Some 1,Book_Type_Multi,x.m8)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecA,1,OrdBuy,5,Some 1,Book_Type_Implied,x.m9)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecA,1,OrdBuy,5,Some 1,Book_Type_Multi,x.m10)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecA,1,OrdSell,1,Some 1,Book_Type_Implied,x.m11)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecA,1,OrdSell,1,Some 1,Book_Type_Multi,x.m12)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecA,1,OrdSell,2,Some 1,Book_Type_Implied,x.m13)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecA,1,OrdSell,2,Some 1,Book_Type_Multi,x.m14)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecA,1,OrdSell,3,Some 1,Book_Type_Implied,x.m15)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecA,1,OrdSell,3,Some 1,Book_Type_Multi,x.m16)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecA,1,OrdSell,4,Some 1,Book_Type_Implied,x.m17)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecA,1,OrdSell,4,Some 1,Book_Type_Multi,x.m18)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecA,1,OrdSell,5,Some 1,Book_Type_Implied,x.m19)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecA,1,OrdSell,5,Some 1,Book_Type_Multi,x.m20)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecB,1,OrdBuy,1,Some 1,Book_Type_Implied,x.m21)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecB,1,OrdBuy,1,Some 1,Book_Type_Multi,x.m22)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecB,1,OrdBuy,2,Some 1,Book_Type_Implied,x.m23)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecB,1,OrdBuy,2,Some 1,Book_Type_Multi,x.m24)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecB,1,OrdBuy,3,Some 1,Book_Type_Implied,x.m25)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecB,1,OrdBuy,3,Some 1,Book_Type_Multi,x.m26)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecB,1,OrdBuy,4,Some 1,Book_Type_Implied,x.m27)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecB,1,OrdBuy,4,Some 1,Book_Type_Multi,x.m28)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecB,1,OrdBuy,5,Some 1,Book_Type_Implied,x.m29)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecB,1,OrdBuy,5,Some 1,Book_Type_Multi,x.m30)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecB,1,OrdSell,1,Some 1,Book_Type_Implied,x.m31)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecB,1,OrdSell,1,Some 1,Book_Type_Multi,x.m32)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecB,1,OrdSell,2,Some 1,Book_Type_Implied,x.m33)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecB,1,OrdSell,2,Some 1,Book_Type_Multi,x.m34)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecB,1,OrdSell,3,Some 1,Book_Type_Implied,x.m35)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecB,1,OrdSell,3,Some 1,Book_Type_Multi,x.m36)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecB,1,OrdSell,4,Some 1,Book_Type_Implied,x.m37)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecB,1,OrdSell,4,Some 1,Book_Type_Multi,x.m38)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecB,1,OrdSell,5,Some 1,Book_Type_Implied,x.m39)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Add(make_ord_add_data(SecB,1,OrdSell,5,Some 1,Book_Type_Multi,x.m40)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Change(make_ord_change_data(SecA,Book_Type_Implied,OrdSell,3,x.m41)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Change(make_ord_change_data(SecB,Book_Type_Implied,OrdBuy,2,x.m42)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Change(make_ord_change_data(SecB,Book_Type_Implied,OrdBuy,4,x.m43)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Change(make_ord_change_data(SecA,Book_Type_Implied,OrdBuy,4,x.m44)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Change(make_ord_change_data(SecB,Book_Type_Multi,OrdSell,5,x.m45)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Change(make_ord_change_data(SecB,Book_Type_Implied,OrdBuy,3,x.m46)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Change(make_ord_change_data(SecA,Book_Type_Implied,OrdSell,1,x.m47)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Change(make_ord_change_data(SecA,Book_Type_Multi,OrdSell,3,x.m48)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Change(make_ord_change_data(SecA,Book_Type_Implied,OrdBuy,3,x.m49)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Change(make_ord_change_data(SecA,Book_Type_Multi,OrdSell,1,x.m50)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Change(make_ord_change_data(SecA,Book_Type_Implied,OrdSell,5,x.m51)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Change(make_ord_change_data(SecB,Book_Type_Multi,OrdBuy,3,x.m52)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Change(make_ord_change_data(SecA,Book_Type_Multi,OrdSell,1,x.m53)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Change(make_ord_change_data(SecB,Book_Type_Multi,OrdBuy,4,x.m54)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Change(make_ord_change_data(SecB,Book_Type_Multi,OrdBuy,3,x.m55)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Change(make_ord_change_data(SecB,Book_Type_Implied,OrdSell,3,x.m56)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Change(make_ord_change_data(SecB,Book_Type_Implied,OrdSell,4,x.m57)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Change(make_ord_change_data(SecB,Book_Type_Multi,OrdSell,5,x.m58)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Change(make_ord_change_data(SecA,Book_Type_Multi,OrdSell,5,x.m59)));
    ExchangeAction(ST_DataSendInc);
    BookAction(ST_Change(make_ord_change_data(SecB,Book_Type_Multi,OrdBuy,2,x.m60)));
    ExchangeAction(ST_DataSendInc);
];;

let   run_all actions = run   ( Some init_ex_state, search_space_to_list actions);;
let valid_all actions = valid ( Some init_ex_state, search_space_to_list actions);;

:shadow off
let n = ref 0;;
let write_ocaml actions =
    let final_state = run_all actions in
    match final_state with 
    | None -> " **** Ignoring empty test case ***** " |> print_string
    | Some final_state ->
    let () = n := !n + 1 in
    let filename = Printf.sprintf "temp/state_a5b87582c247008aaa12d182dbd1164c_%d.ml" !n in
    final_state |> exchange_state_to_ocaml
                |> output_string (open_out filename) 
;;
:shadow on

:adts on
:max_region_time 120
:testgen run_all assuming valid_all with_code write_ocaml

