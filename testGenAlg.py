import argparse
import hashlib
from aglebraic_codegen import *

ocamlTemplate = """:load {model}

{space_defs}

let   run_all actions = run   ( Some init_ex_state, search_space_to_list actions);;
let valid_all actions = valid ( Some init_ex_state, search_space_to_list actions);;

:shadow off
let n = ref 0;;
let write_ocaml actions =
    let final_state = run_all actions in
    match final_state with 
    | None -> " **** Ignoring empty test case ***** " |> print_string
    | Some final_state ->
    let () = n := !n + 1 in
    let filename = Printf.sprintf "{outDir}/state_{filename}_%d.ml" !n in
    final_state |> exchange_state_to_ocaml
                |> output_string (open_out filename) 
;;
:shadow on

:adts on
:max_region_time 120
:testgen run_all assuming valid_all with_code write_ocaml
"""

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Imandra test-generator-generator')
    parser.add_argument('--outDir', default="generatedStates")
    parser.add_argument('--model')
    args = parser.parse_args()
   
    to_expand = {
        "oa_order_qty"  : [ 1 ], 
        "oa_sec_type"   : [ "SecA", "SecB" ],
        "oa_book_type"  : [ "Book_Type_Implied", "Book_Type_Multi" ],
        "oa_level_num"  : [ 1, 2, 3, 4, 5 ],
        "oa_level_side" : [ "OrdBuy", "OrdSell" ],
        "oa_num_orders" : [ "Some 1" ]
    }
    make_book = expand_sequence(
        to_expand,
        constructors=["BookAction","ST_Add"],
        types={"oa_price": "int"}, 
        type="ord_add_data",
        append = Sequence(constructors=["ExchangeAction", "ST_DataSendInc"])
    )
    
    
    to_expand = {
        "oc_sec_type"   : [ "SecA", "SecB" ],
        "oc_book_type"  : [ "Book_Type_Implied", "Book_Type_Multi" ],
        "oc_level_num"  : [ 1, 2, 3, 4, 5 ],
        "oc_level_side" : [ "OrdBuy", "OrdSell" ],
    }
    modify_book = expand_alternative(
        to_expand,
        constructors=[ "BookAction" , "ST_Change" ],
        types={"oc_new_qty": "int"}, 
        type="ord_change_data",
        append = Sequence(constructors=["ExchangeAction", "ST_DataSendInc"])
    )
    
    
    data = sample( make_book >> 6 * modify_book )
    space_defs = generate_code(data)

    print ocamlTemplate.format(
        model=args.model,
        space_defs=space_defs,
        outDir=args.outDir,
        filename=hashlib.md5(space_defs).hexdigest()
    )
