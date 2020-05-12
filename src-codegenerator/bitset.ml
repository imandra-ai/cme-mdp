open Utils 

type bit = {
    bit_name: string;
    bit_desc: string;
    bit_number: int
}

let bit_of_xml node =
    let dict = dict_of_attrs node in 
    {
        bit_name   = get    dict "name";
        bit_desc   = getopt dict "description" "";
        bit_number = xml_value node |> int_of_string
    }

type t = {
    name: string;
    description: string;
    encoding_type: string;
    bits: bit list
}

let from_xml_node node =
    let dict = dict_of_attrs node in 
    {
        name          = get    dict "name";
        description   = getopt dict "description" "";
        encoding_type = get    dict "encodingType";
        bits          = node |> Xml.children 
                             |> List.map bit_of_xml
    }

let type_name t = "t_" ^ t.name
let bits t = 
    t.bits |> List.map (fun b ->
        ("r_" ^ t.name ^ "_" ^ b.bit_name, b)   
    )
let encoding_type t = t.encoding_type 

let string_converter t = "string_of_" ^ t.name
let reader           t = "read_" ^ t.name
let writer           t = "write_" ^ t.name

let ocaml_writer t bitsname varname =
    let idt = "    " in
    let c l = String.concat " " l in
    let et_name = encoding_type t in
    let write_bits_func = "write_bits_" ^ et_name in
    let rec collect currentn = function
        | (name, b)::tl when b.bit_number = currentn -> (varname ^ "." ^ name)::(collect (currentn+1) tl)
        | (name, b)::tl -> "false"::(collect (currentn+1) ((name,b)::tl) )
        | [] -> [] in
    let lst = "[" ^ (bits t |> collect 0 |> String.concat (";\n" ^ idt) ) ^ "]" in
    c [write_bits_func; bitsname; lst ]



let to_ocaml _context t =
    let idt = "    " in
    let c l = String.concat " " l in
    let et_name = encoding_type t in
    let read_func  = "read_" ^ et_name in
    let bit_func   = "bit_" ^ et_name in
    let bit_name  (n, _b) = idt ^ c [ n; ":"; "bool"] in
    let bit_entry (n, b) = idt ^ idt ^ c [ n; "="; bit_func; "v"; string_of_int b.bit_number ] in
    let _lst_entry (n, b) = idt ^ idt ^ c [ n; "="; bit_func; "v"; string_of_int b.bit_number ] in
    let type_decl = 
        c [ "type"; type_name t; "="; "{"; "\n" ] ^ 
        ( bits t |> List.map bit_name |> String.concat ";\n" ) ^ "\n}" in
    let reader_decl =
        c [ "let"; reader t; "bits"; "="; "\n"] ^
        idt ^ c [ "let"; "v, bits"; "="; read_func; "bits"; "in"; "\n" ] ^
        idt ^ "{" ^ "\n" ^
            (bits t |> List.map bit_entry |> String.concat ";\n") ^ "\n" ^
        idt ^ "}, bits" in
    let writer_decl =
        c [ "let"; writer t; "bits"; "v"; "="; "\n"] ^
        idt ^ ocaml_writer t "bits" "v"
    in
    {
        typedecl = type_decl; 
        reader = reader_decl;
        writer = writer_decl
    }
