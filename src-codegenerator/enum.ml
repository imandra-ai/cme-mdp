open Utils 

type case = {
    case_name: string;
    case_desc: string;
    case_id: string
}

let case_of_xml node =
    let dict = dict_of_attrs node in 
    {
        case_name = get    dict "name";
        case_desc = getopt dict "description" "";
        case_id   = xml_value node
    }

type t = {
    name: string;
    description: string;
    encoding_type: string;
    cases: case list
}

let from_xml_node node =
    let dict = dict_of_attrs node in 
    {
        name          = get    dict "name";
        description   = getopt dict "description" "";
        encoding_type = get    dict "encodingType";
        cases         = node |> Xml.children 
                             |> List.map case_of_xml
    }

let type_name t = "t_" ^ t.name
let encoding_type t = t.encoding_type 
let cases t = 
    t.cases |> List.map (fun c -> 
        ("V_" ^ t.name ^ "_" ^ c.case_name, c) 
    ) 

let string_converter t = "string_of_" ^ t.name
let reader           t = "read_" ^ t.name
let writer           t = "write_" ^ t.name


let ocaml_type t et = 
    let idt = "    " in
    let c l = String.concat " " l in
    let null_variant = match Simple.null_value et with
        | Some _ -> Some ("V_" ^ t.name ^ "_Null")
        | None   -> None in
    let null_decl = match null_variant with 
        | Some v -> "\n" ^ c [ idt; "|"; v] 
        | None -> "" in
    let variant_name  (n, _) = c [ idt; "|"; n] in
        ( cases t |> List.map variant_name |> String.concat "\n" ) ^
        null_decl 
 
let ocaml_reader t et =  
    let idt = "    " in
    let c l = String.concat " " l in
    let null_variant, null_value = match Simple.null_value et with
        | Some v -> Some ("V_" ^ t.name ^ "_Null"), v 
        | None   -> None, "" in
    let null_case = match null_variant with 
        | Some v -> c [idt; "|"; "\"" ^ null_value ^ "\""; "->"; v; ","; "bits"; "\n"] 
        | None -> "" in
    let read_func  = Simple.reader et in
    let string_of_func   = Simple.string_of et in
    let case_entry (n, e) = c [ idt; "|"; "\"" ^ e.case_id ^ "\""; "->"; n; ","; "bits"] in
    c [ idt; "let"; "caseid,bits"; "="; read_func; "bits"; "in"; "\n"] ^
    c [ idt; "match"; string_of_func; "caseid"; "with"; "\n"] ^
    (cases t |> List.map case_entry |> String.concat "\n") ^ "\n" ^
    null_case ^
    c [ idt; "|"; "_"; "->"; "raise (Failure (\"Match error in " ^ type_name t ^ "\"))"] 
 

let ocaml_writer t et bitsname varname =  
    let idt = "    " in
    let c l = String.concat " " l in
    let write_func  = Simple.writer et in
    let string_to_func   = Simple.string_to et in
    let null_variant, null_value = match Simple.null_value et with
        | Some v -> Some ("V_" ^ t.name ^ "_Null"), v 
        | None   -> None, "" in
    let null_case = match null_variant with 
        | Some v -> c [idt; "|"; v; "->";  "\"" ^ null_value ^ "\""; "|>"; string_to_func; "|>"; write_func; bitsname; "\n"] 
        | None -> "" in
    let case_entry (n, e) = c [ idt; "|"; n; "->"; "\"" ^ e.case_id ^ "\""; "|>"; string_to_func; "|>"; write_func; bitsname] in
    c [ idt; "match"; varname; "with"; "\n"] ^
    (cases t |> List.map case_entry |> String.concat "\n") ^ "\n" ^
    null_case 
 



let to_ocaml context t =
    let c l = String.concat " " l in
    let et = Hashtbl.find context (encoding_type t) in
    let _null_variant, _null_value = match Simple.null_value et with
        | Some v -> Some ("V_" ^ t.name ^ "_Null"), v | None -> None, "" in
    let type_decl   = c [ "type"; type_name t; "=" ] ^ "\n" ^ ocaml_type t et in
    let reader_decl = c ["let"; reader t; "bits"; "="; "\n"] ^ ocaml_reader t et in
    let writer_decl = c ["let"; writer t; "bits"; "v"; "="; "\n"] ^ ocaml_writer t et "bits" "v" in
    { 
        typedecl = type_decl; 
        reader = reader_decl;
        writer = writer_decl
    }
