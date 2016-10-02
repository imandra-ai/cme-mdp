(**    

    Aesthetic Integration Ltd.
    Copyright 2016
    
    CME_json.ml

*)

(* @meta[imandra_ignore] on @end *)
    open CME_Types;;

    (* The next two lines redefine OCaml's curried max and min  
       to Imandra's tuple-based max and min*)
    let max (a,b) = max a b;;
    let min (a,b) = min a b;;
(* @meta[imandra_ignore] off @end *)

let intopt_to_json : ( int option -> Yojson.Basic.json ) = 
    function Some x -> `Int x | None -> `Assoc []
;;

let intopt_to_ocaml : ( int option -> string ) = 
    function Some x -> Printf.sprintf "Some %d" x | None -> "None" 
;;

let intopt_of_json : ( Yojson.Basic.json -> int option) = 
    function `Int x -> Some x | _ -> None 
;;

(* NOTE: I'm using List.assoc here -- I'm not sure whether I should try Hashtbl. *)
(* But worth trying and benchmarking. *)

(* ****  Serializers for msg_type  **** *)
let msgtype_to_json, msgtype_of_json =
    let m2j : (msg_type * Yojson.Basic.json ) list = [
        ( V_MDUpdateAction_New        , `String "New"        );
        ( V_MDUpdateAction_Change     , `String "Change"     );
        ( V_MDUpdateAction_Delete     , `String "Delete"     );
        ( V_MDUpdateAction_DeleteThru , `String "DeleteThru" );
        ( V_MDUpdateAction_DeleteFrom , `String "DeleteFrom" );
        ( V_MDUpdateAction_Overlay    , `String "Overlay"    ) ] in
    let j2m = List.map (fun (a,b) -> b,a ) m2j in
    (fun x -> List.assoc x m2j), (fun x -> List.assoc x j2m  )
;;
let msgtype_to_ocaml : ( msg_type -> string ) = function
    | V_MDUpdateAction_New        -> "V_MDUpdateAction_New"       
    | V_MDUpdateAction_Change     -> "V_MDUpdateAction_Change"    
    | V_MDUpdateAction_Delete     -> "V_MDUpdateAction_Delete"    
    | V_MDUpdateAction_DeleteThru -> "V_MDUpdateAction_DeleteThru"
    | V_MDUpdateAction_DeleteFrom -> "V_MDUpdateAction_DeleteFrom"
    | V_MDUpdateAction_Overlay    -> "V_MDUpdateAction_Overlay"   
;;


(* ****  Serializers for entry_type  **** *)
let entrytype_to_json, entrytype_of_json = 
    let e2j : ( entry_type * Yojson.Basic.json ) list = [
        ( V_MDEntryType_Bid          , `String "Bid"          );
        ( V_MDEntryType_Offer        , `String "Offer"        );
        ( V_MDEntryType_ImpliedBid   , `String "ImpliedBid"   );
        ( V_MDEntryType_ImpliedOffer , `String "ImpliedOffer" );
        ( V_MDEntryType_EmptyBook    , `String "EmptyBook"    ) ] in
    let j2e = List.map (fun (a,b) -> b,a ) e2j in
    (fun x -> List.assoc x e2j), (fun x -> List.assoc x j2e )   
;;
let entrytype_to_ocaml : ( entry_type -> string ) = function
    | V_MDEntryType_Bid          -> "V_MDEntryType_Bid"         
    | V_MDEntryType_Offer        -> "V_MDEntryType_Offer"       
    | V_MDEntryType_ImpliedBid   -> "V_MDEntryType_ImpliedBid"  
    | V_MDEntryType_ImpliedOffer -> "V_MDEntryType_ImpliedOffer"
    | V_MDEntryType_EmptyBook    -> "V_MDEntryType_EmptyBook"   
;;

(* ****  Serializers for channel_type  **** *)
let channel_to_json, channel_of_json = 
    let c2j : ( channel_type * Yojson.Basic.json ) list = [
        ( Ch_Ref_A  , `String "RefreshA"  );
        ( Ch_Ref_B  , `String "RefreshB"  );
        ( Ch_Snap_A , `String "SnapshotA" );
        ( Ch_Snap_B , `String "SnapshotB" ) ] in
    let j2c = List.map (fun (a,b) -> b,a ) c2j in
    (fun x -> List.assoc x c2j), (fun x -> List.assoc x j2c )   
;;
let channel_to_ocaml : ( channel_type -> string ) = function
    | Ch_Ref_A  ->  "Ch_Ref_A" 
    | Ch_Ref_B  ->  "Ch_Ref_B" 
    | Ch_Snap_A ->  "Ch_Snap_A"
    | Ch_Snap_B ->  "Ch_Snap_B"
;;

(* **** Serializers for order_level **** *)
let order_level_to_json : (order_level -> Yojson.Basic.json) = function 
    | NoLevel -> `Assoc []
    | Level o -> `Assoc [
        ("Side",      match o.side with OrdBuy -> `String "BUY" | OrdSell -> `String "SELL");
        ("Quantity", `Int o.qty   );
        ("Price",    `Int o.price  );
        ("NumOrders", match o.num_orders with None -> `Assoc [] | Some n -> `Int n )
    ]
;; 
let order_level_of_json : ( Yojson.Basic.json -> order_level ) = 
    let open Yojson.Basic.Util in
    function
    | `Assoc [] -> NoLevel
    | j -> Level { 
        qty        = j |> member "Quantity"  |> to_int;
        price      = j |> member "Price"     |> to_int;
        num_orders = j |> member "NumOrders" |> to_int_option;
        side       = j |> member "Side"      |>  (function 
            | `String "BUY"  -> OrdBuy 
            | `String "SELL" -> OrdSell
            | _ -> failwith "Side should be either BUY or SELL" )
    } 
;; 
let order_level_to_ocaml : ( order_level -> string ) = function 
    | NoLevel -> "NoLevel"
    | Level l -> 
        let side = match l.side with OrdBuy -> "OrdBuy" | OrdSell -> "OrdSell" in
        Printf.sprintf "Level { qty=%d; price=%d; num_orders=%s; side=%s }"
            l.qty l.price (intopt_to_ocaml l.num_orders) side
;;

(* ****  Serializers for book   **** *)
let book_to_json : (book -> Yojson.Basic.json ) =
   function { buys = buys; sells = sells } -> `Assoc [
        ( "Buys" , `List ( List.map order_level_to_json buys  ) ); 
        ( "Sells", `List ( List.map order_level_to_json sells ) )
    ]
;;

let book_of_json : ( Yojson.Basic.json -> book ) =
    let open Yojson.Basic.Util in
    function j -> { 
        buys  = j |> member "Buys"  |> to_list |> List.map order_level_of_json; 
        sells = j |> member "Sells" |> to_list |> List.map order_level_of_json
    }
;;    

let book_to_ocaml ( b : book ) : string =
    let buys  = "[" ^ String.concat ";" (List.map order_level_to_ocaml b.buys  ) ^ "]" in
    let sells = "[" ^ String.concat ";" (List.map order_level_to_ocaml b.sells ) ^ "]" in
    Printf.sprintf "{ buys=%s; sells=%s }" buys sells
;;


(* ****  Serializers for inc_refresh   **** *)
let inc_refresh_to_json : ref_message -> Yojson.Basic.json = fun p -> `Assoc [
    ( "SequrityID", `Int p.rm_security_id );
    ( "RepSeqNum",  `Int p.rm_rep_seq_num );
    ( "PriceLevel", `Int p.rm_price_level );
    ( "EntrySize",  `Int p.rm_entry_size  );
    ( "EntryPrice", `Int p.rm_entry_px    );
    ( "NumOrders",   intopt_to_json p.rm_num_orders     );
    ( "MessageType", msgtype_to_json   p.rm_msg_type    );
    ( "EntryType",   entrytype_to_json p.rm_entry_type  ) 
];;
let inc_refresh_of_json (j : Yojson.Basic.json) : ref_message = 
    let open Yojson.Basic.Util in
    let to_int j = to_int j |> fun x -> max (0,x) in  
    let cap255 j = min (j, 255) in {
        rm_security_id = j |> member "SequrityID"  |> to_int;
        rm_rep_seq_num = j |> member "RepSeqNum"   |> to_int;
        rm_price_level = j |> member "PriceLevel"  |> to_int |> cap255 ;  (* NOTE: fix for pricelevel above 255*)
        rm_entry_size  = j |> member "EntrySize"   |> to_int;
        rm_entry_px    = j |> member "EntryPrice"  |> to_int;
        rm_num_orders  = j |> member "NumOrders"   |> intopt_of_json;
        rm_msg_type    = j |> member "MessageType" |> msgtype_of_json;
        rm_entry_type  = j |> member "EntryType"   |> entrytype_of_json
    };;
let inc_refresh_to_ocaml : ref_message -> string = 
    fun p -> "{" ^ String.concat ";" [ 
        Printf.sprintf "rm_security_id=%d"p.rm_security_id ;
        Printf.sprintf "rm_rep_seq_num=%d"p.rm_rep_seq_num ;
        Printf.sprintf "rm_price_level=%d"p.rm_price_level ;
        Printf.sprintf "rm_entry_size=%d" p.rm_entry_size  ;
        Printf.sprintf "rm_entry_px=%d"   p.rm_entry_px    ;
        Printf.sprintf "rm_num_orders=%s" (   intopt_to_ocaml p.rm_num_orders);
        Printf.sprintf "rm_msg_type=%s"   (  msgtype_to_ocaml p.rm_msg_type  );
        Printf.sprintf "rm_entry_type=%s" (entrytype_to_ocaml p.rm_entry_type)
    ] ^ "}";;


(* ****  Serializers for message   **** *)
let message_to_json (message : message) : Yojson.Basic.json  =
   let convert_snap_message : snap_message -> Yojson.Basic.json =
        fun p -> `Assoc [
            ( "SecurityID", `Int p.sm_security_id );
            ( "RepSeqNum",  `Int p.sm_rep_seq_num );
            ( "LastMsgSeqNumProcessed",  `Int p.sm_snapshot.snap_last_msg_seq_num_processed );
            ( "MultiBook"   , book_to_json p.sm_snapshot.snap_m_book );
            ( "ImpliedBook" , book_to_json p.sm_snapshot.snap_i_book )
        ] in
    match message with
        | RefreshMessage  m -> inc_refresh_to_json  m 
        | SnapshotMessage m -> convert_snap_message m
;;
        
let message_of_json ( json : Yojson.Basic.json) : message =
    let open Yojson.Basic.Util in
    (* NOTE : Cutting ALL negative integers across the board *)
    let read_snap j = {
            sm_security_id = j |> member "SecurityID" |> to_int ;
            sm_rep_seq_num = j |> member "RepSeqNum"  |> to_int ;
            sm_snapshot = {
                snap_last_msg_seq_num_processed = j |> member "LastMsgSeqNumProcessed" |> to_int;
                snap_m_book  = j |> member "MultiBook"   |> book_of_json;
                snap_i_book  = j |> member "ImpliedBook" |> book_of_json
            }
        } in
    if member "MultiBook" json != `Null then SnapshotMessage  (read_snap json )
                                        else RefreshMessage   (inc_refresh_of_json  json)
;;
let message_to_ocaml (message : message) : string =
   let convert_snap_message : snap_message -> string =
        fun p -> "{" ^ String.concat ";" [
            Printf.sprintf "sm_security_id=%d" p.sm_security_id;
            Printf.sprintf "sm_rep_seq_num=%d" p.sm_rep_seq_num;
            Printf.sprintf "sm_snapshot={snap_last_msg_seq_num_processed=%d;snap_m_book=%s;snap_i_book=%s}"
                p.sm_snapshot.snap_last_msg_seq_num_processed 
                ( book_to_ocaml p.sm_snapshot.snap_m_book )
                ( book_to_ocaml p.sm_snapshot.snap_i_book )
        ] ^ "}" in
    match message with
    | SnapshotMessage m -> "SnapshotMessage" ^ convert_snap_message m
    |  RefreshMessage m ->  "RefreshMessage" ^ inc_refresh_to_ocaml  m
;;
        
        
let packet_to_json ( packet : packet ) : Yojson.Basic.json  =
    `Assoc [
        ( "SeqNum"   , `Int packet.packet_seq_num );
        ( "Channel"  , packet.packet_channel |> channel_to_json );
        ( "Messages" , `List ( packet.packet_messages |> List.map message_to_json ))
    ] 
;;
let packet_of_json (json : Yojson.Basic.json) : packet  =
    let open Yojson.Basic.Util in {
        packet_seq_num  = json |> member "SeqNum"   |> to_int;
        packet_channel  = json |> member "Channel"  |> channel_of_json;
        packet_messages = json |> member "Messages" |> ( function
            | `List msgs -> List.map message_of_json msgs
            | _ -> failwith "Failed to parse a packet."
        )
    }
;;
let packet_to_ocaml (packet : packet) : string  =
    "{" ^ String.concat ";" [
    Printf.sprintf "packet_seq_num=%d" packet.packet_seq_num;
    Printf.sprintf "packet_channel=%s" (channel_to_ocaml packet.packet_channel);
    Printf.sprintf "packet_messages=[%s]" ( 
        packet.packet_messages |> List.map message_to_ocaml
                               |> String.concat ";"
    )] ^ "}"
;;

let packets_to_json (packets : packet list) : Yojson.Basic.json  =
    (* This weirdness is just for performance reasons *)
    let rec tail_map current = function
        | h::tl -> tail_map ( packet_to_json h :: current ) tl
        | [] -> List.rev current
        in
    `Assoc [( "packets", `List ( tail_map [] packets ))] 
;;
let packets_of_json (json : Yojson.Basic.json) : packet list =
    let open Yojson.Basic in 
    match json with
        | `Assoc [( "packets", `List packets)] -> List.map packet_of_json packets
        | _ -> raise (Failure "A single toplevel \"packets\" key is expected.")
;;
let packets_to_ocaml (packets : packet list) : string = "[" ^ (
        packets |> List.map packet_to_ocaml
                |> String.concat ";" 
    ) ^ "]";;

