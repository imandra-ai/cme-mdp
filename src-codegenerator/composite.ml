open Utils

type t = {
    name: string;
    description: string;
    records: Simple.t list
}

let from_xml_node node =
    let dict = dict_of_attrs node in 
    {
        name        = get    dict "name";
        description = getopt dict "description" "";
        records     = node |> Xml.children 
                           |> List.map Simple.from_xml_node
    }

let type_name t = "t_" ^ t.name
let fields t = 
    t.records |> List.map (fun e ->
        ("f_" ^ t.name ^ "_" ^ Simple.name e , e)  
    )

let string_converter t = "string_of_" ^ t.name
let reader           t = "read_" ^ t.name
let writer           t = "write_" ^ t.name

let to_ocaml context t =
    let idt = "    " in
    let c l = String.concat " " l in
    let def_entry (n, t) = c [ idt; n; ":"; Simple.ocaml_type t] in 
    let let_entry (n, t) = c [ idt; "let"; n; ","; "bits"; "="; Simple.ocaml_reader t "bits"; "in"] in 
    let put_entry (n, t) = c [ idt; "let"; "bits"; "="; Simple.ocaml_writer t "bits" ("v." ^ n) ; "in"] in 
    let type_decl = 
        c [ "type"; type_name t; "="; "{\n" ] ^
        ( fields t |> List.map def_entry |> String.concat ";\n" ) ^ "\n}" in
    let reader_decl = 
        c ["let"; reader t; "bits"; "="; "\n"] ^
        ( fields t |> List.map let_entry |> String.concat "\n" ) ^ "\n" ^
        idt ^ "{ " ^
        ( fields t |> List.map fst |> String.concat ";") ^ "}, bits" in
    let writer_decl = 
        c ["let"; writer t; "bits"; "v"; "="; "\n"] ^
        ( fields t |> List.map put_entry |> String.concat "\n" ) ^ "\n" ^
        idt ^ "bits" in
    { 
        typedecl = type_decl; 
        reader = reader_decl;
        writer = writer_decl 
    }



