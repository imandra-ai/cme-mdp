import argparse
import hashlib
from aglebraic_codegen import *

ocamlTemplate = """:load {model}

{state}

{space_defs}

let   run_all actions = run   ( Some start_state, search_space_to_list actions);;
let valid_all actions = valid ( Some start_state, search_space_to_list actions);;

:shadow off
let n = ref 0;;
let write_ocaml actions =
    let final_state = run_all actions in
    match final_state with 
    | None -> " **** Ignoring empty test case ***** " |> print_string
    | Some final_state ->
    let () = n := !n + 1 in
    let filename = Printf.sprintf "{outDir}/state_{filename}_%d.ml" !n in
    let file = open_out filename in
    let () = final_state 
        |> exchange_state_to_ocaml
        |> Printf.sprintf "let start_state = %s;;"
        |> output_string file 
        in
    close_out file
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
    parser.add_argument('--state')
    args = parser.parse_args()

    send_snaps =               Sequence( constructors=["ExchangeAction", "ST_Snapshot", "SecB" ] )
    send_snaps = send_snaps >> Sequence( constructors=["ExchangeAction", "ST_DataSendSnap"     ] )
 
    inc  = Sequence(constructors=["ExchangeAction", "ST_DataSendInc"])
    snap = Sequence(constructors=["ExchangeAction", "ST_Snapshot", "SecB"]) 
    snap = snap >> Sequence( constructors=["ExchangeAction", "ST_DataSendSnap"])
   
   
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
        t=True,
        append = inc | snap | (inc >> snap)
    )
    
    to_expand = {
        "od_sec_type"   : [ "SecA", "SecB" ],
        "od_book_type"  : [ "Book_Type_Implied", "Book_Type_Multi" ],
        "od_level_side" : [ "OrdBuy", "OrdSell" ] 
    }
    modify_book = modify_book | expand_alternative(
        to_expand,
        constructors=[ "BookAction", "ST_Delete" ],
        types={"od_level_num":"int"},
        type="ord_del_data",
        t=True,
        append = inc | snap | (inc >> snap)
    )

    data = sample( send_snaps >> 15*modify_book >> send_snaps )
    space_defs = generate_code(data)

    with open(args.state, "r") as sFile:
        state = "let start_state={0}\n;;".format( sFile.read() )
        
    print ocamlTemplate.format(
        state=state,
        model=args.model,
        space_defs=space_defs,
        outDir=args.outDir,
        filename=hashlib.md5(space_defs).hexdigest()
    )
