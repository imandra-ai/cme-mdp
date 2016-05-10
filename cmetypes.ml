open Utils 

type t =
    | Simple    of Simple.t
    | Composite of Composite.t
    | Enum      of Enum.t
    | BitSet    of Bitset.t

let parse_type node =
    match Xml.tag node with 
    | "type"      -> Simple    ( Simple.from_xml_node    node )
    | "composite" -> Composite ( Composite.from_xml_node node )
    | "enum"      -> Enum      ( Enum.from_xml_node      node )
    | "set"       -> BitSet    ( Bitset.from_xml_node    node )
    |  wat        -> raise (Failure ("Unknown type tag <" ^ wat ^ "> encountered."));;

let to_ocaml context_hash = function
    | Simple    x -> Simple.to_ocaml    context_hash x
    | Composite x -> Composite.to_ocaml context_hash x
    | Enum      x -> Enum.to_ocaml      context_hash x 
    | BitSet    x -> Bitset.to_ocaml    context_hash x


let get_types filename = 
    let rec search = function
        | Xml.Element ( "types" , _ , children )::tl -> children
        | hd::tl -> search tl
        | [] -> raise (Failure "Types tag not found")
    in
    filename |> Xml.parse_file |> Xml.children 
             |> search         |> List.map parse_type

