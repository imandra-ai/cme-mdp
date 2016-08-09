let sectype_to_json, sectype_of_json =
    let s2j : (sec_type * Yojson.Basic.json ) list = [
        ( SecA , `String "SecA" );
        ( SecB , `String "SecB" ) ] in
    let j2s = List.map (fun (a,b) -> b,a ) s2j in
    (fun x -> List.assoc x s2j), (fun x -> List.assoc x j2s  )
;;

let booktype_to_json, booktype_of_json =
    let b2j : (book_type * Yojson.Basic.json ) list = [
        ( Book_Type_Implied , `String "BookImplied" );
        ( Book_Type_Multi   , `String "BookMulti"   ) ] in
    let j2b = List.map (fun (a,b) -> b,a ) b2j in
    (fun x -> List.assoc x b2j), (fun x -> List.assoc x j2b  )
;;

let orderside_to_json, orderside_of_json =
    let o2j : (order_side * Yojson.Basic.json ) list = [
        ( OrdBuy  , `String "Buy"  );
        ( OrdSell , `String "Sell" ) ] in
    let j2o = List.map (fun (a,b) -> b,a ) o2j in
    (fun x -> List.assoc x o2j), (fun x -> List.assoc x j2o  )
;;


let add_to_json data = `Assoc [
    (       "Qty" , `Int data.oa_order_qty );
    (     "Price" , `Int data.oa_price     );
    (  "LevelNum" , `Int data.oa_level_num );
    (   "SecType" , data.oa_sec_type   |>   sectype_to_json );
    (  "BookType" , data.oa_book_type  |>  booktype_to_json ); 
    ( "LevelSide" , data.oa_level_side |> orderside_to_json );
    ( "NumOrders" , data.oa_num_orders |> (function Some x -> `Int x | None -> `Assoc []) )
];;

let change_to_json data = `Assoc [
    (       "Qty" , `Int data.oc_new_qty   );
    (  "LevelNum" , `Int data.oc_level_num );
    (   "SecType" , data.oc_sec_type   |>   sectype_to_json );
    (  "BookType" , data.oc_book_type  |>  booktype_to_json ); 
    ( "LevelSide" , data.oc_level_side |> orderside_to_json );
];;

let delete_to_json data = `Assoc [
    (  "LevelNum" , `Int data.od_level_num );
    (   "SecType" , data.od_sec_type   |>   sectype_to_json );
    (  "BookType" , data.od_book_type  |>  booktype_to_json ); 
    ( "LevelSide" , data.od_level_side |> orderside_to_json );
];;


let state_transition_to_json st =
    match st with
    | ST_BookReset     -> `Assoc [ ( "BookReset", `Assoc []             ) ]
    | ST_Add data      -> `Assoc [ ( "Add"      , add_to_json data      ) ]
    | ST_Change data   -> `Assoc [ ( "Change"   , change_to_json data   ) ]  
    | ST_Delete   data -> `Assoc [ ( "Delete"   , delete_to_json data   ) ]
    | ST_Snapshot sec  -> `Assoc [ ( "Snapshot" , sectype_to_json sec   ) ] 
    | ST_DataSendInc   -> `Assoc [ ( "SendIncrementRefresh" , `Assoc [] ) ]
    | ST_DataSendSnap  -> `Assoc [ ( "SendSnapshot"         , `Assoc [] ) ] 
;;


