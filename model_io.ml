open Utils

let ref_assoc = [
    ( "RptSeq"         , "rm_rep_seq_num" );
    ( "SecurityID"     , "rm_security_id" );
    ( "MDPriceLevel"   , "rm_price_level" );
    ( "MDEntryType"    , "rm_entry_type"  );
    ( "MDEntrySize"    , "rm_entry_size"  );
    ( "MDEntryPx"      , "rm_entry_px"    );
    ( "NumberOfOrders" , "rm_num_orders"  );
    ( "MDUpdateAction" , "rm_msg_type"    )
]

let generate_ref_of m =
    let open Cmemessages in
    let idt = "   " in 
    let c = String.concat " " in
    let nomdentries_group = 
        m.entries 
            |> List.find ( function 
                         | Field _ -> false 
                         | Group g -> g.Group.name = "NoMDEntries" ) 
            |> (function | Group g -> g | _ -> raise (Failure "Wat?") )
        in
    let fields = nomdentries_group.Group.fields |> List.filter 
                 ( fun f -> List.mem_assoc f.Field.name ref_assoc ) in
    let () = if List.length fields = List.length ref_assoc then () 
             else raise (Failure "Not all fields found") in
    let ename = Group.fname m.name nomdentries_group in
    String.concat "\n" [
        c [ "let"; "ref_of_" ^ string_of_int m.id; "x"; "="];
        c [   idt; "let"; "mk_entry"; "e"; "="; "{" ] ;
        fields  |> List.map ( fun f -> 
                    c [ idt; idt; List.assoc f.Field.name ref_assoc; 
                        "="; "e.Message_types." ^ ename f; ";"] )
                |> String.concat "\n";
        c [ idt; "}"; "in" ]
    ]
        

