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

(******************* Writers *************************)

let add_to_json data : Yojson.Basic.json = `Assoc [
    (       "Qty" , `Int data.oa_order_qty );
    (     "Price" , `Int data.oa_price     );
    (  "LevelNum" , `Int data.oa_level_num );
    (   "SecType" , data.oa_sec_type   |>   sectype_to_json );
    (  "BookType" , data.oa_book_type  |>  booktype_to_json ); 
    ( "LevelSide" , data.oa_level_side |> orderside_to_json );
    ( "NumOrders" , data.oa_num_orders |> (function Some x -> `Int x | None -> `Assoc []) )
];;

let change_to_json data : Yojson.Basic.json = `Assoc [
    (       "Qty" , `Int data.oc_new_qty   );
    (  "LevelNum" , `Int data.oc_level_num );
    (   "SecType" , data.oc_sec_type   |>   sectype_to_json );
    (  "BookType" , data.oc_book_type  |>  booktype_to_json ); 
    ( "LevelSide" , data.oc_level_side |> orderside_to_json );
];;

let delete_to_json data : Yojson.Basic.json = `Assoc [
    (  "LevelNum" , `Int data.od_level_num );
    (   "SecType" , data.od_sec_type   |>   sectype_to_json );
    (  "BookType" , data.od_book_type  |>  booktype_to_json ); 
    ( "LevelSide" , data.od_level_side |> orderside_to_json );
];;

let state_transition_to_json st : Yojson.Basic.json =
    match st with
    | ST_BookReset     -> `Assoc [ ( "BookReset", `Assoc []             ) ]
    | ST_Add data      -> `Assoc [ ( "Add"      , add_to_json data      ) ]
    | ST_Change data   -> `Assoc [ ( "Change"   , change_to_json data   ) ]  
    | ST_Delete   data -> `Assoc [ ( "Delete"   , delete_to_json data   ) ]
    | ST_Snapshot sec  -> `Assoc [ ( "Snapshot" , sectype_to_json sec   ) ] 
    | ST_DataSendInc   -> `Assoc [ ( "SendIncrementRefresh" , `Assoc [] ) ]
    | ST_DataSendSnap  -> `Assoc [ ( "SendSnapshot"         , `Assoc [] ) ] 
;;

(******************* Readers *************************)

let add_of_json ( json : Yojson.Basic.json ) = 
    let open Yojson.Basic.Util in {
    oa_order_qty  = json |> member "Qty"       |> to_int ;
    oa_price      = json |> member "Price"     |> to_int ;
    oa_level_num  = json |> member "LevelNum"  |> to_int ;
    oa_sec_type   = json |> member "SecType"   |>   sectype_of_json ;
    oa_book_type  = json |> member "BookType"  |>  booktype_of_json ; 
    oa_level_side = json |> member "LevelSide" |> orderside_of_json ;
    oa_num_orders = json |> member "NumOrders" |> (function `Int x -> Some x | _ -> None) 
};;

let change_of_json ( json : Yojson.Basic.json ) =
    let open Yojson.Basic.Util in {
    oc_new_qty    = json |> member "Qty"       |> to_int ;
    oc_level_num  = json |> member "LevelNum"  |> to_int ;
    oc_sec_type   = json |> member "SecType"   |>   sectype_of_json ;
    oc_book_type  = json |> member "BookType"  |>  booktype_of_json ; 
    oc_level_side = json |> member "LevelSide" |> orderside_of_json ;
};;

let delete_of_json ( json : Yojson.Basic.json ) = 
    let open Yojson.Basic.Util in {
    od_level_num  = json |> member "LevelNum"  |> to_int ;
    od_sec_type   = json |> member "SecType"   |>   sectype_of_json ;
    od_book_type  = json |> member "BookType"  |>  booktype_of_json ; 
    od_level_side = json |> member "LevelSide" |> orderside_of_json ;
};;


let state_transition_of_json ( json : Yojson.Basic.json ) =
    match json with
    | `Assoc [ ( "BookReset", `Assoc [] ) ]  -> ST_BookReset     
    | `Assoc [ ( "Add"      , data      ) ]  -> ST_Add    (    add_of_json data )
    | `Assoc [ ( "Change"   , data      ) ]  -> ST_Change ( change_of_json data )    
    | `Assoc [ ( "Delete"   , data      ) ]  -> ST_Delete ( delete_of_json data ) 
    | `Assoc [ ( "Snapshot" , sec       ) ]  -> ST_Snapshot ( sectype_of_json sec )  
    | `Assoc [ ( "SendIncrementRefresh" , `Assoc [] ) ]  -> ST_DataSendInc   
    | `Assoc [ ( "SendSnapshot"         , `Assoc [] ) ]  -> ST_DataSendSnap  
    | _ -> failwith "Unrecognized exchange transition entry."              
;;

