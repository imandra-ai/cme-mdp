open Utils

module Field = struct
    type t = {
        id: int;
        name: string;
        description: string;
        cmetype: string
    }

    let from_xml_node node =
        let dict = dict_of_attrs node in 
        {
            id           = get    dict "id" |> int_of_string;
            name         = get    dict "name";
            description  = getopt dict "description" "";
            cmetype      = get    dict "type";
        }

    let name m t = "f_" ^ m ^ "_" ^ t.name

end

module Group = struct
    type t = {
        id: int;
        name: string;
        description: string;
        block_length: int;
        dimension_type: string;
        fields: Field.t list
    }

    let from_xml_node node =
        let dict = dict_of_attrs node in 
        {
            id             = get    dict "id" |> int_of_string;
            name           = get    dict "name";
            description    = getopt dict "description" "";
            block_length   = get    dict "blockLength" |> int_of_string;
            dimension_type = get    dict "dimensionType";
            fields         = node |> Xml.children
                                  |> List.map Field.from_xml_node
        }

    let name  m t = "g_" ^ m ^ "_" ^ t.name
    let fname m t e = "f_" ^ m ^ "_" ^ t.name ^ "_" ^ e.Field.name

    let to_ocaml m t = 
        let get_entry e =  "    " ^ (fname m t e) ^ ": t_" ^ e.cmetype in
        let let_entry e =  "    let " ^ (fname m t e) ^ ", bits = read_" ^ e.cmetype ^ " bits in" in
        let put_entry e =  "    let nbits = write_" ^ e.Field.cmetype ^ " nbits v." ^ (fname m t e) ^ " in" in
        { 
            typedecl = "type " ^ (name m t) ^ " = {\n" ^
                         ( t.fields |> List.map get_entry |> String.concat ";\n") ^ "\n}";
            reader = "let read_" ^ name m t ^  " bits = \n" ^
                         ( t.fields |> List.map let_entry |> String.concat "\n") ^ "\n" ^
                     "    {" ^ (t.fields |> List.map (fname m t) |> String.concat "; ") ^ "}, bits";
            writer = "let write_" ^ name m t ^  " n bits v = \n" ^
                     "    let nbits = Binparser.create_out () in \n" ^
                         ( t.fields |> List.map put_entry |> String.concat "\n") ^ "\n" ^
                     "    Binparser.append_padded bits nbits n \n"
        }
        
end

type entry = 
    | Field of Field.t 
    | Group of Group.t

let mk_entry node =
    match Xml.tag node with
    | "field" -> Field( Field.from_xml_node node )
    | "group" -> Group( Group.from_xml_node node )
    | x -> raise (Failure ("Unrecognized tag <" ^ x ^ "> in a message." ))

type t = {
    id: int;
    name: string;
    description: string;
    block_length: int;
    entries: entry list
}

let from_xml_node node =
    let dict = dict_of_attrs node in 
    {
        id           = get    dict "id" |> int_of_string;
        name         = get    dict "name";
        description  = getopt dict "description" "";
        block_length = get    dict "blockLength" |> int_of_string;
        entries      = node |> Xml.children
                            |> List.map mk_entry
    }

let name t = "msg_" ^ t.name

let to_ocaml t = 
    if t.entries = [] then {
        typedecl = "type " ^ name t ^ " = unit \n";
        reader = "let read_msg_" ^ t.name ^ " (bits, gbits) = () \n";
        writer = "let write_msg_" ^ t.name ^ " x = Binparser.create_out (), Binparser.create_out () \n" 
    } else
    let rec predefine_groups = function
        | (Field _)::tl -> predefine_groups tl
        | (Group x)::tl -> codecat (Group.to_ocaml t.name x) (predefine_groups tl)
        | [] -> { typedecl = ""; writer = ""; reader = "" } 
    in
    let fname = function | Field x -> Field.name t.name x | Group x -> "f_" ^ t.name ^ "_" ^ x.name in
    let entry_ocaml = function 
        | Field x -> "    " ^ (Field.name t.name x) ^ ": t_" ^ x.cmetype
        | Group x -> "    f_" ^ t.name ^ "_" ^ x.name ^ ": " ^ Group.name t.name x ^ " list"
    in
    let func_ocaml = function 
        | Field x -> "    let " ^ (Field.name t.name x) ^ ", bits = read_" ^ x.cmetype ^ " bits in"
        | Group x -> "    let f_" ^ t.name ^ "_" ^ x.name ^ ", gbits = \n" ^ 
                     "         let nent, bsz, gbits = Binparser.get_group_info gbits in\n" ^
                     "         foldi (nent, bsz) read_" ^ Group.name t.name x ^ " gbits in"
    in
    let put_ocaml = function 
        | Field x -> "    let bits = write_" ^ x.cmetype ^ " bits " ^ "v." ^ (Field.name t.name x) ^ " in"
        | Group x -> "    let gbits = \n" ^ 
                     "         let gbits = Binparser.write_group_info  gbits ( " ^ 
                                    "List.length v.f_" ^ t.name ^ "_" ^ x.name ^ " , " ^
                                    string_of_int x.block_length ^ " ) in\n" ^
                     "         List.fold_left ( write_" ^ Group.name t.name x ^ " " ^ 
                               string_of_int x.block_length ^ " ) gbits v.f_" ^ t.name ^ "_" ^ x.name ^ " in"
    in
    let grps = predefine_groups t.entries in
    {
        typedecl = String.concat "\n" [
            grps.typedecl;
            "type " ^ "msg_" ^ t.name ^ " = { \n" ^ 
                ( t.entries |> List.map entry_ocaml |> String.concat ";\n" ) ^ 
            "\n}" ];
        reader = String.concat "\n" [ 
            grps.reader;
            "let " ^ "read_msg_" ^ t.name ^ " (bits, gbits) = \n" ^
                 ( t.entries |> List.map func_ocaml |> String.concat "\n" ) ^ "\n" ^
            "    { " ^ ( t.entries |> List.map fname |> String.concat "; " ) ^ " } "
            ];
        writer = String.concat "\n" [ 
            grps.writer;
            "let " ^ "write_msg_" ^ t.name ^ " v = \n" ^
            "    " ^ "let bits, gbits = Binparser.create_out (), Binparser.create_out () in\n" ^
                 ( t.entries |> List.map put_ocaml |> String.concat "\n" ) ^ "\n" ^ 
            "    " ^ "bits, gbits"
        ]
    }

let get_types filename = 
    let rec scan nlist = 
        match nlist with
        | Xml.Element ( "ns2:message" , _ , _ )::tl -> 
            (from_xml_node (List.hd nlist))::(scan tl)
        | _::tl -> scan tl
        | [] -> []
    in
    filename |> Xml.parse_file |> Xml.children |> scan

