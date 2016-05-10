(**    

    Aesthetic Integration Ltd.
    Copyright 2016
    
    CME_json.ml

*)


(* NOTE: I'm using List.assoc here -- I'm not sure whether I should try Hashtbl. *)
(* But worth trying and benchmarking. *)

let msgtype_to_json, msgtype_of_json =
    let m2j : ( CME.msg_type * Yojson.Basic.json ) list = [
        ( CME.Action_New        , `String "New"        );
        ( CME.Action_Change     , `String "Change"     );
        ( CME.Action_Delete     , `String "Delete"     );
        ( CME.Action_DeleteThru , `String "DeleteThru" );
        ( CME.Action_DeleteFrom , `String "DeleteFrom" );
        ( CME.Action_Overlay    , `String "Overlay"    ) ] in
    let j2m = List.map (fun (a,b) -> b,a ) m2j in
    (fun x -> List.assoc x m2j), (fun x -> List.assoc x j2m  )

let entrytype_to_json, entrytype_of_json = 
    let e2j : ( CME.entry_type * Yojson.Basic.json ) list = [
        ( CME.Entry_Bid                      , `String "Bid"          );
        ( CME.Entry_Offer                    , `String "Offer"        );
        ( CME.Entry_ImpliedBid               , `String "ImpliedBid"   );
        ( CME.Entry_ImpliedOffer             , `String "ImpliedOffer" );
        ( CME.Entry_EmptyBook                , `String "EmptyBook"    ) ] in
    let j2e = List.map (fun (a,b) -> b,a ) e2j in
    (fun x -> List.assoc x e2j), (fun x -> List.assoc x j2e )   

let book_to_json : ( CME.book -> Yojson.Basic.json ) =
    let convert_order_level : (CME.order_level -> Yojson.Basic.json) = function 
        | CME.NoLevel -> `Assoc []
        | CME.Level o -> `Assoc [
            ("Side",      match o.CME.side with CME.BUY -> `String "BUY" | CME.SELL -> `String "SELL");
            ("Quantity", `Int o.CME.qty   );
            ("Price",    `Int o.CME.price  );
            ("NumOrders", match o.CME.num_orders with None -> `Assoc [] | Some n -> `Int n )
        ] in 
    function { CME.buys = buys; CME.sells = sells } -> `Assoc [
        ( "Buys" , `List ( List.map convert_order_level buys  ) ); 
        ( "Sells", `List ( List.map convert_order_level sells ) )
    ]
    
 let book_of_json : ( Yojson.Basic.json -> CME.book ) =
    let open Yojson.Basic.Util in
    let read_order_level : ( Yojson.Basic.json -> CME.order_level ) = function 
        | `Assoc [] -> CME.NoLevel
        | j -> CME.Level { 
            CME.qty        = j |> member "Quantity"  |> to_int;
            CME.price      = j |> member "Price"     |> to_int;
            CME.num_orders = j |> member "NumOrders" |> to_int_option;
            CME.side       = j |> member "Side"      |>  (function 
                | `String "BUY"  -> CME.BUY 
                | `String "SELL" -> CME.SELL
                | _ -> failwith "Side should be either BUY or SELL" )
        } in 
    function j -> { 
        CME.buys  = j |> member "Buys"  |> to_list |> List.map read_order_level; 
        CME.sells = j |> member "Sells" |> to_list |> List.map read_order_level
    }
    
    

let packet_to_json (packet : CME.packet) : Yojson.Basic.json  =
    let convert_header : CME.packet_header -> Yojson.Basic.json = 
        fun h -> `Assoc [
            ("SequenceNumber", `Int h.CME.ph_packet_seq_num);
            ("SendingTime",    `Int h.CME.ph_sending_time)
        ] in  
    let convert_inc_refresh : CME.ref_message -> Yojson.Basic.json = 
        fun p -> `Assoc [
            ( "SequrityID", `Int p.CME.rm_security_id );
            ( "RepSeqNum",  `Int p.CME.rm_rep_seq_num );
            ( "PriceLevel", `Int p.CME.rm_price_level );
            ( "EntrySize",  `Int p.CME.rm_entry_size  );
            ( "EntryPrice", `Int p.CME.rm_entry_px    );
            ( "NumOrders",  `Int p.CME.rm_num_orders  );
            ( "MessageType", msgtype_to_json   p.CME.rm_msg_type    );
            ( "EntryType",   entrytype_to_json p.CME.rm_entry_type  ) 
        ] in
   let convert_snap_message : CME.snap_message -> Yojson.Basic.json =
        let open CME in
        fun p -> `Assoc [
            ( "SecurityID", `Int p.sm_security_id );
            ( "RepSeqNum",  `Int p.sm_rep_seq_num );
            ( "LastMsgSeqNumProcessed",  `Int p.sm_last_msg_seq_num_processed );
            ( "MultiBook"   , book_to_json p.sm_snapshot.snap_m_book );
            ( "ImpliedBook" , book_to_json p.sm_snapshot.snap_i_book )
        ] in
    match packet with
        | CME.SnapshotPacket   p -> `Assoc [ ( "SnapshotPacket" ,   
            `Assoc [ ( "Header" , convert_header p.CME.sp_header  );
                     ( "Message", convert_snap_message p.CME.sp_snap ) ] 
            ) ]
        | CME.IncRefreshPacket p -> `Assoc [ ( "IncRefreshPacket" , 
            `Assoc [ ( "Header" , convert_header p.CME.rp_header  );
                     ( "Message", convert_inc_refresh p.CME.rp_msg ) ] 
            ) ]
        | CME.NoPacket -> `Assoc []
        
    
let packet_of_json ( json : Yojson.Basic.json) : CME.packet   =
    let open Yojson.Basic.Util in
    (* NOTE : Cutting ALL negative integers across the board *)
    let to_int j = to_int j |> max 0 in 
    let read_header j = {
            CME.ph_packet_seq_num = j |> member "SequenceNumber" |> to_int;
            CME.ph_sending_time   = j |> member "SendingTime"    |> to_int 
        } in
    let read_inc j = {
            CME.rm_security_id = j |> member "SequrityID"  |> to_int;
            CME.rm_rep_seq_num = j |> member "RepSeqNum"   |> to_int;
            CME.rm_price_level = j |> member "PriceLevel"  |> to_int |> min 255;  (* NOTE: fix for pricelevel above 255*)
            CME.rm_entry_size  = j |> member "EntrySize"   |> to_int;
            CME.rm_entry_px    = j |> member "EntryPrice"  |> to_int;
            CME.rm_num_orders  = j |> member "NumOrders"   |> to_int;
            CME.rm_msg_type    = j |> member "MessageType" |> msgtype_of_json;
            CME.rm_entry_type  = j |> member "EntryType"   |> entrytype_of_json
        } in
    let read_snap j = let open CME in {
            sm_security_id = j |> member "SecurityID" |> to_int ;
            sm_rep_seq_num = j |> member "RepSeqNum"  |> to_int ;
            sm_last_msg_seq_num_processed = j |> member "LastMsgSeqNumProcessed" |> to_int;
            sm_snapshot = {
                snap_seq_num = j |> member "LastMsgSeqNumProcessed" |> to_int; (* NOTE: That is kinda redundant ... *)
                snap_m_book  = j |> member "MultiBook"   |> book_of_json;
                snap_i_book  = j |> member "ImpliedBook" |> book_of_json
            }
        } in
    match json with
        | `Assoc [ ( "SnapshotPacket" ,   `Assoc lst ) ] ->
            let h, m = List.assoc "Header" lst, List.assoc "Message" lst in
            CME.SnapshotPacket   { CME.sp_header = read_header h; CME.sp_snap = read_snap m }
        | `Assoc [ ( "IncRefreshPacket" , `Assoc lst ) ] ->
            let h, m = List.assoc "Header" lst, List.assoc "Message" lst in
            CME.IncRefreshPacket { CME.rp_header = read_header h; CME.rp_msg  = read_inc m }
        | `Assoc [] -> CME.NoPacket
        | _ -> failwith "Unrecognized packet entry."              
        
        
let packets_to_json (packets : CME.packet list) : Yojson.Basic.json  =
    let rec tail_map current = function
        | h::tl -> tail_map ( packet_to_json h :: current ) tl
        | [] -> List.rev current
        in
    `Assoc [( "packets", `List ( tail_map [] packets ))] 

let packets_of_json (json : Yojson.Basic.json) : CME.packet list =
    let open Yojson.Basic in 
    match json with
        | `Assoc [( "packets", `List packets)] -> List.map packet_of_json packets
        | _ -> raise (Failure "A single toplevel \"packets\" key is expected.")



