import argparse
import hashlib
from aglebraic_codegen import *

ocamlTemplate = """:load {model}

:load {state}

{space_defs}

let run_all actions = 
    let net_state={{incoming=start_state.pac_queue; outgoing=[]; cache=[]}} in 
    run ( net_state, search_space_to_list actions)
;;

let valid_all actions = 
    let net_state={{incoming=start_state.pac_queue; outgoing=[]; cache=[]}} in 
    valid ( net_state, search_space_to_list actions)
;;

:shadow off
let n = ref 0;;
let write_ocaml actions =
    let net_state = run_all actions in
    let final_state = {{ start_state with pac_queue = net_state }} in
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


    ne = Sequence(constructors=["NoEffect"])
    network = ne | ne | ne | ne | ne
    network = network | Sequence(constructors=["PacketLoss"])
    network = network | Sequence(constructors=["PacketMoveToCache"])
    network = network | Sequence(
        constructors=["PacketMoveFromCache"],
        types={"net_from_cache": "int"},
        type="net_from_cache"
    )
    
   
    data = sample( 30 * network )
    space_defs = generate_code(data)

    print ocamlTemplate.format(
        model=args.model,
        state=args.state,
        space_defs=space_defs,
        outDir=args.outDir,
        filename=hashlib.md5(space_defs).hexdigest()
    )
