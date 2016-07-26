(**    

    Aesthetic Integration Ltd.
    Copyright 2016
    
    CME_json.ml

*)

let intopt_to_json : ( int option -> Yojson.Basic.json ) = 
    function Some x -> `Int x | None -> `Assoc []
;;

let intopt_of_json : ( Yojson.Basic.json -> int option) = 
    function `Int x -> Some x | _ -> None 
;;

(* NOTE: I'm using List.assoc here -- I'm not sure whether I should try Hashtbl. *)
(* But worth trying and benchmarking. *)

let msgtype_to_json, msgtype_of_json =
    let open CME_Types in
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


let entrytype_to_json, entrytype_of_json = 
    let open CME_Types in
    let e2j : ( entry_type * Yojson.Basic.json ) list = [
        ( V_MDEntryType_Bid                      , `String "Bid"          );
        ( V_MDEntryType_Offer                    , `String "Offer"        );
        ( V_MDEntryType_ImpliedBid               , `String "ImpliedBid"   );
        ( V_MDEntryType_ImpliedOffer             , `String "ImpliedOffer" );
        ( V_MDEntryType_EmptyBook                , `String "EmptyBook"    ) ] in
    let j2e = List.map (fun (a,b) -> b,a ) e2j in
    (fun x -> List.assoc x e2j), (fun x -> List.assoc x j2e )   
;;


let channel_to_json, channel_of_json = 
    let open CME_Types in
    let c2j : ( channel_type * Yojson.Basic.json ) list = [
        ( Ch_Ref_A  , `String "RefreshA"  );
        ( Ch_Ref_B  , `String "RefreshB"  );
        ( Ch_Snap_A , `String "SnapshotA" );
        ( Ch_Snap_B , `String "SnapshotB" ) ] in
    let j2c = List.map (fun (a,b) -> b,a ) c2j in
    (fun x -> List.assoc x c2j), (fun x -> List.assoc x j2c )   
;;


let book_to_json : ( CME_Types.book -> Yojson.Basic.json ) =
    let open CME_Types in
    let convert_order_level : (order_level -> Yojson.Basic.json) = function 
        | None -> `Assoc []
        | Some o -> `Assoc [
            ("Side",      match o.side with OrdBuy -> `String "BUY" | OrdSell -> `String "SELL");
            ("Quantity", `Int o.qty   );
            ("Price",    `Int o.price  );
            ("NumOrders", match o.num_orders with None -> `Assoc [] | Some n -> `Int n )
        ] in 
    function { buys = buys; sells = sells } -> `Assoc [
        ( "Buys" , `List ( List.map convert_order_level buys  ) ); 
        ( "Sells", `List ( List.map convert_order_level sells ) )
    ]
;;
    

let book_of_json : ( Yojson.Basic.json -> CME_Types.book ) =
    let open CME_Types in
    let open Yojson.Basic.Util in
    let read_order_level : ( Yojson.Basic.json -> order_level ) = function 
        | `Assoc [] -> None
        | j -> Some { 
            qty        = j |> member "Quantity"  |> to_int;
            price      = j |> member "Price"     |> to_int;
            num_orders = j |> member "NumOrders" |> to_int_option;
            side       = j |> member "Side"      |>  (function 
                | `String "BUY"  -> OrdBuy 
                | `String "SELL" -> OrdSell
                | _ -> failwith "Side should be either BUY or SELL" )
        } in 
    function j -> { 
        buys  = j |> member "Buys"  |> to_list |> List.map read_order_level; 
        sells = j |> member "Sells" |> to_list |> List.map read_order_level
    }
;;    
    

let message_to_json (message : CME_Types.message) : Yojson.Basic.json  =
    let open CME_Types in
    let convert_inc_refresh : ref_message -> Yojson.Basic.json = 
        fun p -> `Assoc [
            ( "SequrityID", `Int p.rm_security_id );
            ( "RepSeqNum",  `Int p.rm_rep_seq_num );
            ( "PriceLevel", `Int p.rm_price_level );
            ( "EntrySize",  `Int p.rm_entry_size  );
            ( "EntryPrice", `Int p.rm_entry_px    );
            ( "NumOrders",   intopt_to_json p.rm_num_orders     );
            ( "MessageType", msgtype_to_json   p.rm_msg_type    );
            ( "EntryType",   entrytype_to_json p.rm_entry_type  ) 
        ] in
   let convert_snap_message : snap_message -> Yojson.Basic.json =
        fun p -> `Assoc [
            ( "SecurityID", `Int p.sm_security_id );
            ( "RepSeqNum",  `Int p.sm_rep_seq_num );
            ( "LastMsgSeqNumProcessed",  `Int p.sm_snapshot.snap_last_msg_seq_num_processed );
            ( "MultiBook"   , book_to_json p.sm_snapshot.snap_m_book );
            ( "ImpliedBook" , book_to_json p.sm_snapshot.snap_i_book )
        ] in
    match message with
        | RefreshMessage  m -> convert_inc_refresh  m 
        | SnapshotMessage m -> convert_snap_message m
;;
        
    
let message_of_json ( json : Yojson.Basic.json) : CME_Types.message =
    let open Yojson.Basic.Util in
    let open CME_Types in
    (* NOTE : Cutting ALL negative integers across the board *)
    let to_int j = to_int j |> max 0 in 
    let read_inc j = {
            rm_security_id = j |> member "SequrityID"  |> to_int;
            rm_rep_seq_num = j |> member "RepSeqNum"   |> to_int;
            rm_price_level = j |> member "PriceLevel"  |> to_int |> min 255;  (* NOTE: fix for pricelevel above 255*)
            rm_entry_size  = j |> member "EntrySize"   |> to_int;
            rm_entry_px    = j |> member "EntryPrice"  |> to_int;
            rm_num_orders  = j |> member "NumOrders"   |> intopt_of_json;
            rm_msg_type    = j |> member "MessageType" |> msgtype_of_json;
            rm_entry_type  = j |> member "EntryType"   |> entrytype_of_json
        } in
    let read_snap j = {
            sm_security_id = j |> member "SecurityID" |> to_int ;
            sm_rep_seq_num = j |> member "RepSeqNum"  |> to_int ;
            sm_snapshot = {
                snap_last_msg_seq_num_processed = j |> member "LastMsgSeqNumProcessed" |> to_int;
                snap_m_book  = j |> member "MultiBook"   |> book_of_json;
                snap_i_book  = j |> member "ImpliedBook" |> book_of_json
            }
        } in
    match json with
        | `Assoc [ ( "SnapshotMessage" , m ) ] -> SnapshotMessage  (read_snap m )
        | `Assoc [ ( "RefreshMessage"  , m ) ] -> RefreshMessage   (read_inc  m )
        | _ -> failwith "Unrecognized message entry."              
;;
        
        
let packet_to_json ( packet : CME_Types.packet ) : Yojson.Basic.json  =
    `Assoc [
        ( "SeqNum"   , `Int packet.packet_seq_num );
        ( "Channel"  , packet.packet_channel |> channel_to_json );
        ( "Messages" , `List ( packet.packet_messages |> List.map message_to_json ))
    ] 
;;


let packet_of_json (json : Yojson.Basic.json) : CME_Types.packet  =
    let open Yojson.Basic.Util in
    let open CME_Types in {
        packet_seq_num  = json |> member "SeqNum"   |> to_int;
        packet_channel  = json |> member "Channel"  |> channel_of_json;
        packet_messages = json |> member "Messages" |> ( function
            | `List msgs -> List.map message_of_json msgs
            | _ -> failwith "Failed to parse a packet."
        )
    }
;;

let packets_to_json (packets : CME_Types.packet list) : Yojson.Basic.json  =
    (* This weirdness is just for performance reasons *)
    let rec tail_map current = function
        | h::tl -> tail_map ( packet_to_json h :: current ) tl
        | [] -> List.rev current
        in
    `Assoc [( "packets", `List ( tail_map [] packets ))] 
;;

let packets_of_json (json : Yojson.Basic.json) : CME_Types.packet list =
    let open Yojson.Basic in 
    match json with
        | `Assoc [( "packets", `List packets)] -> List.map packet_of_json packets
        | _ -> raise (Failure "A single toplevel \"packets\" key is expected.")
;;

