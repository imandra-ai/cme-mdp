type code = {
    typedecl: string;
    reader: string;
    writer: string;
}

let codecat a b = {
    typedecl = a.typedecl ^ "\n" ^ b.typedecl;
    reader   = a.reader   ^ "\n" ^ b.reader;
    writer   = a.writer   ^ "\n" ^ b.writer
}


(* XML helper *)

let xml_value node =
    match Xml.children node with
    | (Xml.PCData v)::[] -> v 
    | _ -> raise (Failure ("Expected a single value in the <" ^ Xml.tag node ^ "> tag."))

(* Helpers for working with the dictionary of attrs *)

module StrMap = Map.Make(String)

let dict_of_attrs node =
    node |> Xml.attribs
         |> List.fold_left ( fun m (k,v) -> StrMap.add k v m) StrMap.empty 

let get dict key = StrMap.find key dict

let getopt dict key default = 
    if StrMap.mem key dict then get dict key else default
 
