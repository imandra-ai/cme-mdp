
(* @meta[imandra_ignore] on @end *)
open CME_json;;
open CME_Types;;
open CME_Exchange;;
(* @meta[imandra_ignore] off @end *)


(* *** order_book *** *)
let order_book_to_json book : Yojson.Basic.json = 
    let book_side_to_json side = `Assoc [
        ( "One"   , side.one   |> order_level_to_json ) ;
        ( "Two"   , side.two   |> order_level_to_json ) ;
        ( "Three" , side.three |> order_level_to_json ) ;
        ( "Four"  , side.four  |> order_level_to_json ) ;
        ( "Five"  , side.five  |> order_level_to_json ) 
    ] in `Assoc [
        (  "BuyOrders" , book.buy_orders  |> book_side_to_json );
        ( "SellOrders" , book.sell_orders |> book_side_to_json )
    ]
;;

let order_book_of_json ( json : Yojson.Basic.json ) : order_book = 
    let open Yojson.Basic.Util in 
    let book_side_of_json json = {
        one   = json |> member "One"   |> order_level_of_json;
        two   = json |> member "Two"   |> order_level_of_json;
        three = json |> member "Three" |> order_level_of_json;
        four  = json |> member "Four"  |> order_level_of_json;
        five  = json |> member "Five"  |> order_level_of_json
    } in {
        buy_orders  = json |> member  "BuyOrders" |> book_side_of_json;
        sell_orders = json |> member "SellOrders" |> book_side_of_json
    }
;;

let order_book_to_ocaml book : string = 
    let book_side_to_json side = "{" ^ String.concat ";" [ 
        Printf.sprintf   "one=%s" ( side.one   |> order_level_to_ocaml ) ;
        Printf.sprintf   "two=%s" ( side.two   |> order_level_to_ocaml ) ;
        Printf.sprintf "three=%s" ( side.three |> order_level_to_ocaml ) ;
        Printf.sprintf  "four=%s" ( side.four  |> order_level_to_ocaml ) ;
        Printf.sprintf  "five=%s" ( side.five  |> order_level_to_ocaml ) 
    ] ^ "}" in 
    Printf.sprintf "{ buy_orders=%s; sell_orders=%s }"        
        ( book.buy_orders  |> book_side_to_json )
        ( book.sell_orders |> book_side_to_json )
;;


(* **** Serializing security state **** *)
let security_state_to_json (st : security_state ) : Yojson.Basic.json = `Assoc [
    ( "LastRepSeqNumber" , `Int st.last_rep_seq_num );
    ( "SecurityID"       , `Int st.sec_id           );
    ( "MultiBook"        , st.multi_book   |> order_book_to_json );
    ( "ImpliedBook"      , st.implied_book |> order_book_to_json )
];;

let security_state_of_json (json : Yojson.Basic.json) : security_state = 
    let open Yojson.Basic.Util in {
    last_rep_seq_num = json |> member "LastRepSeqNumber" |> to_int;
    sec_id = json |> member "SecurityID" |> to_int;
    multi_book   = json |> member "MultiBook"   |> order_book_of_json ;
    implied_book = json |> member "ImpliedBook" |> order_book_of_json 
};;

let security_state_to_ocaml (st : security_state ) : string = 
    "{" ^ String.concat ";" [ 
        Printf.sprintf "last_rep_seq_num=%d" st.last_rep_seq_num;
        Printf.sprintf "sec_id=%d" st.sec_id;
        Printf.sprintf "multi_book=%s"   ( st.multi_book   |> order_book_to_ocaml );
        Printf.sprintf "implied_book=%s" ( st.implied_book |> order_book_to_ocaml )
] ^ "}";;

(* **** Serailizer for exchange state ****  *)
let exchange_state_to_json state : Yojson.Basic.json = `Assoc [
    ( "SecurityStateA" , state.sec_a     |> security_state_to_json );
    ( "SecurityStateB" , state.sec_b     |> security_state_to_json );
    ( "PacketQueue"    , state.pac_queue |> packets_to_json        );
    ( "LastIncSequenceNumber"  , `Int state.last_inc_seq_num       );
    ( "LastSnapSequenceNumber" , `Int state.last_snap_seq_num      );
];;

let exchange_state_of_json (json : Yojson.Basic.json) : exchange_state = 
    let open Yojson.Basic.Util in {
    sec_a = json |> member "SecurityStateA" |> security_state_of_json;
    sec_b = json |> member "SecurityStateB" |> security_state_of_json;
    pac_queue = json |> member "PacketQueue" |> packets_of_json;
    last_inc_seq_num  = json |> member "LastIncSequenceNumber"  |> to_int;
    last_snap_seq_num = json |> member "LastSnapSequenceNumber" |> to_int;
     inc_msg_queue = [];
    snap_msg_queue = [];
    num_resets = 0
};;

let exchange_state_to_ocaml state  =
  Printf.sprintf "{ %s }"
    (String.concat ";"
       [ Printf.sprintf "sec_a=%s"     ( state.sec_a |> security_state_to_ocaml )
       ; Printf.sprintf "sec_b=%s"     ( state.sec_b |> security_state_to_ocaml )
       ; Printf.sprintf "pac_queue=%s" ( state.pac_queue |> packets_to_ocaml    )
       ; Printf.sprintf "last_inc_seq_num=%d"   state.last_inc_seq_num
       ; Printf.sprintf "last_snap_seq_num=%d"  state.last_snap_seq_num
       ; "inc_msg_queue=[]"
       ; "snap_msg_queue=[]"
       ; "num_resets=0"
       ])
;;

let network_state_ocaml_of_exchange_state state =
  Printf.sprintf "{ %s }"
    (String.concat "; "
       [ Printf.sprintf "incoming=List.rev ( %s )" ( state.pac_queue |> packets_to_ocaml )
       ; "outgoing=[]"
       ; "cache=[]"
       ])
;;

let json_string_of_network_state state =
  `Assoc
    [ ( "PacketQueue"    , state.outgoing |> packets_to_json        );
    ]
  |> Yojson.Basic.to_string
;;









(*


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

*)
