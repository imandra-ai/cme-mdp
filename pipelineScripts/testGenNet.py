import argparse
import hashlib

ocamlTemplate = """:load {model}

:load {state}

{space_defs}

let start_net_state={{
    incoming=List.rev start_state.pac_queue; outgoing=[]; cache=[]
}};;

let run_all actions = run ( start_net_state, search_space_to_list actions) ;;

let valid_all actions = valid ( start_net_state, search_space_to_list actions) ;;

:shadow off
let n = ref 0;;
let write_json actions =
    let net_state = run_all actions in
    let final_state = {{ start_state with 
        pac_queue = ( List.rev net_state.incoming ) @ net_state.outgoing
    }} in
    let () = n := !n + 1 in
    let filename = Printf.sprintf "{outDir}/state_{filename}_%d.json" !n in
    final_state 
        |> exchange_state_to_json
        |> Yojson.Basic.to_file filename 
;;
:shadow on

:adts on
:max_region_time 120
:testgen run_all assuming valid_all with_code write_json
"""

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Imandra test-generator-generator')
    parser.add_argument('--outDir', default="generatedStates")
    parser.add_argument('--model')
    parser.add_argument('--state')
    parser.add_argument('--neffects', type=int)
    args = parser.parse_args()


    typedefs  = [ "type search_space = {" ]
    typedefs += [ "m{0}: net_effect;".format(n) for n in range(args.neffects) ]

    letdefs  = [ "let search_space_to_list x = [" ]
    letdefs += [ "x.m{0};".format(n) for n in range(args.neffects) ]

    space_defs  = "\n    ".join(typedefs)
    space_defs += "\n};;\n"
    space_defs += "\n    ".join(letdefs)
    space_defs += "\n];;\n"

    print ocamlTemplate.format(
        model=args.model,
        state=args.state,
        space_defs=space_defs,
        outDir=args.outDir,
        filename=hashlib.md5(space_defs+args.state).hexdigest()
    )
