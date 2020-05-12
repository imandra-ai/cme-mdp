open Utils

type t = {
    name: string;
    description: string;
    primitive_type: string;
    length: int;
    null_value: string option; 
    constant: string option 
}

let from_xml_node node =
    let dict = dict_of_attrs node in 
    let is_const = getopt dict "presence" "" = "constant" in
    {
        name           = get    dict "name";
        description    = getopt dict "description" "";
        primitive_type = get    dict "primitiveType" ;
        length         = getopt dict "length" "1"    |> int_of_string;
        null_value     = getopt dict "nullValue" ""  |> (function "" -> None | x -> Some x );
        constant       = if is_const then Some( xml_value node ) else None 
    }

let name t           = t.name
let type_name t      = "t_" ^ t.name
let primitive_type t = t.primitive_type

let length     t = t.length
let constant   t = t.constant 
let null_value t = t.null_value 

let bit_extractor  t = "bit_" ^ t.name
let bit_writer     t = "write_bits_" ^ t.name
let string_of      t = "string_of_" ^ t.name
let string_to      t = "string_to_" ^ t.name
let reader         t = "read_" ^ t.name
let writer         t = "write_" ^ t.name


type modifier =
    | NoModifier 
    | Nullable of string
    | Listed of int 
    | Constant of string 

let get_modifier t =
    match (length t, null_value t, constant t) with
        | 1 , None   , None   -> NoModifier
        | n , None   , None   -> Listed n
        | 1 , Some v , None   -> Nullable v
        | 1 , None   , Some v -> Constant v
        | _ -> raise ( Failure (
            "In type " ^ type_name t ^ "Only one modifier is allowed " ^
            "(either length>1, nullable or constant) for a basic type." ) )


let ocaml_type t = 
    let ocaml_typename = primitive_type t |> Binparser.string_to_pt |> Binparser.to_ocaml  in
    match get_modifier t with
        | NoModifier -> ocaml_typename            
        | Constant _ -> ocaml_typename             
        | Nullable _ -> ocaml_typename ^ " option" 
        | Listed _   -> ocaml_typename ^ " list"    

let ocaml_reader t varname = 
    let c l = String.concat " " l in
    let pt_name = primitive_type t in
    let string_to_func = "Binparser.string_to_" ^ pt_name in
    let read_func   = "Binparser.read_" ^ pt_name in
    match get_modifier t with
            | NoModifier -> c [ read_func; varname ]
            | Constant v -> c [ "(" ^ string_to_func; "\"" ^ v ^ "\")"; ","; varname ] 
            | Nullable v -> c [ "Binparser.nullable_to_option"; 
                                "("; string_to_func; "\"" ^ v ^ "\")";
                                "("; read_func; varname; ")" ]
            | Listed n   -> c [ "Binparser.repeat"; string_of_int n; read_func; varname] 

let ocaml_writer t bitsname varname= 
    let c l = String.concat " " l in
    let pt_name = primitive_type t in
    let _string_of_func = "Binparser.string_of_" ^ pt_name in
    let string_to_func = "Binparser.string_to_" ^ pt_name in
    let write_func     = "Binparser.write_" ^ pt_name in
    match get_modifier t with
        | NoModifier -> c [ write_func; bitsname; varname ]
        | Constant _ -> c [ bitsname ] 
        | Nullable v -> c [ "match"; varname; "with"; 
                                "|";  "Some"; "x"; "->"; write_func; bitsname; "x";
                                "|";  "None"; "->"; "\"" ^ v ^ "\""; 
                                        "|>"; string_to_func; 
                                        "|>"; write_func; bitsname; ] 
        | Listed _   -> c [ "List.fold_left"; write_func; bitsname; varname ] 

let ocaml_string_of t varname =
    let c l = String.concat " " l in
    let pt_name = primitive_type t in
    let string_of_func = "Binparser.string_of_" ^ pt_name in
    let string_to_func = "Binparser.string_to_" ^ pt_name in
    match get_modifier t with
        | NoModifier -> c [ string_of_func; varname ]
        | Constant v -> c [ "\"" ^ v ^ "\""] 
        | Nullable v -> c [ "match"; varname; "with";
                                "|";  "Some"; "x"; "->"; string_of_func; "x";
                                "|";  "None"; "->"; "\"" ^ v ^ "\""; "|>"; string_to_func; 
                                                                     "|>"; string_of_func ]
        | Listed _   -> c [ varname; "|>"; "List.map"; string_of_func; 
                                     "|>"; "String.concat"; "\"\"" ] 

let ocaml_string_to t varname =
    let c l = String.concat " " l in
    let pt_name = primitive_type t in
    let string_to_func = "Binparser.string_to_" ^ pt_name in
    match get_modifier t with
        | NoModifier -> c [ string_to_func; varname ]
        | Constant v -> c [ "\"" ^ v ^ "\""; "|>"; string_to_func ] 
        | Nullable v -> c [ "match"; varname; "with";
                                "|"; "\"" ^ v ^ "\""; "->"; "None";
                                "|"; "x"; "->"; "Some ("; string_to_func; "x"; ")" ]
        | Listed _   -> c [ "[]" ] 


let to_ocaml context t = 
    Hashtbl.add context (name t) t;
    let c l = String.concat " " l in
    let pt_name = primitive_type t in
    let  wbit_func = "Binparser.write_bits_" ^ pt_name in
    let   bit_func = "Binparser.bit_"  ^ pt_name in
    let _read_func = "Binparser.read_" ^ pt_name in
    let      type_decl = c [ "type"; type_name t; "="; ocaml_type t ] in
    let     write_decl = c [ "let";           writer t; "bits v ="; ocaml_writer    t "bits" "v"; ";;"] in
    let string_of_decl = c [ "let";        string_of t;      "v ="; ocaml_string_of t    "v"; ";;"] in
    let string_to_decl = c [ "let";        string_to t;      "v ="; ocaml_string_to t    "v"; ";;"] in
    let    reader_decl = c [ "let";           reader t;   "bits ="; ocaml_reader    t "bits"; ";;"] in
    let       bit_decl = c [ "let";    bit_extractor t;        "="; bit_func; ";;"] in
    let      wbit_decl = c [ "let";       bit_writer t;        "="; wbit_func; ";;"] in
    let    reader_full = match get_modifier t with
         | NoModifier -> [ reader_decl; string_of_decl; bit_decl ]
         |          _ -> [ reader_decl; string_of_decl ] in
    let    writer_full = match get_modifier t with
         | NoModifier -> [ write_decl; string_to_decl; wbit_decl ]
         | Constant _ -> [ write_decl ]
         | Nullable _ -> [ write_decl; string_to_decl; wbit_decl ]
         | Listed _   -> [ write_decl ] in
 
    {
        typedecl = type_decl;
          reader = String.concat "\n" reader_full;
          writer = String.concat "\n" writer_full
    }






                 
