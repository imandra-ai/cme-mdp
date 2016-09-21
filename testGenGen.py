import json
import hashlib
import itertools
import random
import argparse

# Generates a random subsample from an input list preserves ordering of 
# the entries if shuffle=False. 
def randomSample(lst, N, shuffle=False):
    indices = random.sample(range(len(lst)), N)
    if not shuffle:
        indices = sorted(indices)
    return [lst[i] for i in indices]


# This makes "let make_type" fuction declarations -- those are very useful 
# to in reducing the size fo the generated code. Also they oranize the "combinatorial"
# and the "seach space" declaraions into subsets going a 
# The code itself is placed in the "recordMakers" dictionary.
def create_makers(jsonData,  recordMakers, vsname="x"):
    for entry in jsonData:
        if "entries" in entry:
            recordMakers = create_makers(entry["entries"], recordMakers, vsname)
            continue
        if "records" not in entry: 
            continue
        if "type" not in entry:
            raise KeyError("Both 'record' and 'type' keys shoud be present.")
        if entry["type"] in recordMakers:
            continue # already done
        nrecords = len(entry["records"])

        parameters = ",".join([vsname+str(i+1) for i in range(nrecords)])

        code = []
        code.append("let make_{0} ({1}) = {{".format( entry["type"], parameters))
        n = 0
        for record in entry["records"]:
            if "variants" in record:
                n += 1
                code.append("{0} = {1}{2};".format(record["record"], vsname, n))
        for record in entry["records"]:
            if "variants" not in record:
                n += 1
                code.append("{0} = {1}{2};".format(record["record"], vsname, n))
        code.append("};;")
        recordMakers[entry["type"]] = "\n    ".join(code)
    return recordMakers


# The following two mutually recursive functions simultanteously generate
# the entries in the "search_space" type declaration and 
# the "search_space_to_list" function 
ssname="x"
def product(entries):
    for entry in entries:
        if "entries" in entry:
            for e in expand_block(entry):
                yield e
            continue
            
        if "records" not in entry:
            cs = entry["constructors"]
            yield ([], "(".join(cs) + ")"*(len(cs)-1))
            continue
            
        variants = []
        for record in entry["records"]:
            if "variants" in record:
                variants.append(map(str, record["variants"]))

        for case in itertools.product(*variants):
            search_space_code = []
            mkparams = list(case)
            for record in entry["records"]:
                if "variants" not in record:
                    product.varCounter += 1
                    ss_record = record["record"] + "_" + str(product.varCounter)
                    search_space_code.append(ss_record + ":" + record["type"])
                    mkparams.append(ssname + "." + ss_record)

            constr, closing = "",""
            for c in entry["constructors"] : 
                constr  = constr + c + "("
                closing = closing + ")" 
            constr = constr + "make_" + entry["type"] + "("
            closing = closing + ")" 

            yield ( search_space_code, constr + ",".join(mkparams) + closing )

    raise StopIteration
product.varCounter = 0

    
def expand_block(block):
    codeEntries = list(product(block["entries"]))
    if ("sample" not in block) and ("shuffle" not in block):
        return codeEntries
    shuffle = block.get("shuffle", False)
    size    = block.get("sample",  len(codeEntries))
    return randomSample(codeEntries, size, shuffle=shuffle)


header = '''
#use "topfind";;
#require "yojson";;

:load Model/CME_Types.ml
:load Model/CME_Exchange.ml
:load Model/CME_Network.ml
:load_ocaml Printers/CME_json.ml

type state = {
    exchange_state : exchange_state ;
     network_state :  network_state 
};;

type action =
    | BookAction     of book_transition
    | ExchangeAction of exchange_transition
    | CopyPackets
    | NetworkAction  of net_effect
;;


(* A recursive run function.
   Note how this implicitly includes transition validity.

   @meta[measure : run]
     let measure_run (s, actions) = List.length actions
   @end
*)
let rec run (state, acts) =
    match state, acts with
    |    _ , [] -> state
    | None ,  _ -> state
    | Some s, BookAction act :: acts -> 
        let es = process_book_trans (s.exchange_state, act) in 
        run (Some {s with exchange_state = es}, acts)  
    | Some s, ExchangeAction act :: acts -> 
        let es = process_exchange_trans (s.exchange_state, act) in 
        run (Some {s with exchange_state = es}, acts)
    | Some s, NetworkAction  act :: acts -> 
        let ns = process_net_effect (s.network_state, act) in 
        run (Some { s with network_state = ns } , acts ) 
    | Some s, CopyPackets :: acts ->
        let s = Some { s with network_state = 
            { s.network_state with 
                incoming = s.exchange_state.pac_queue
            }
        } in run ( s , acts)
;;

(* We set up run for staged symbolic execution *)
:stage run

(* @meta[measure : valid]
    let measure_valid (s, actions) = List.length actions
    @end
*)

let rec valid (s, acts) =
    match (s, acts) with 
    | None   , _  -> false 
    | Some _ , [] -> true
    | Some s, BookAction act :: acts ->  
        is_book_trans_valid (s.exchange_state, act) && (
        let es = process_book_trans (s.exchange_state, act) in 
        valid (Some {s with exchange_state = es}, acts) )
    | Some s, ExchangeAction act :: acts -> 
        is_exchange_trans_valid (s.exchange_state, act) && (
        let es = process_exchange_trans (s.exchange_state, act) in 
        valid (Some {s with exchange_state = es}, acts) )
    | Some s,  NetworkAction  act :: acts -> 
        is_neteffect_valid (s.network_state, act) && (
        let ns = process_net_effect (s.network_state, act) in 
        valid (Some {s with network_state = ns}, acts) )
    | Some s, CopyPackets :: acts ->
        let s = Some { s with network_state = 
            { s.network_state with 
                incoming = s.exchange_state.pac_queue
            }
        } in valid ( s , acts)
;;
'''

footer =''' 
let empty_state = Some {{
    exchange_state = init_ex_state;
    network_state = empty_network_state  
}};;

let run_all m = run ( empty_state, search_space_to_list m ) ;;

let valid_all m = valid ( empty_state, search_space_to_list m ) ;;

:shadow off
let n = ref 0;;
let write_jsons m =
    let final_state = run ( empty_state, search_space_to_list m ) in
    match final_state with 
    | None -> " **** Ignoring empty test case ***** " |> print_string
    | Some final_state ->
    let packets = final_state.network_state.outgoing @ final_state.network_state.incoming in
    let () = n := !n + 1 in
    packets |> packets_to_json
            (*|> Yojson.Basic.pretty_to_string |> print_string *)
            |> Yojson.Basic.to_file (Printf.sprintf "generated/test_{0}_%d.json" !n) 
;;
:shadow on
:adts on

:max_region_time 120
:testgen run_all assuming valid_all with_code write_jsons 
'''


def generate_code(jsonFile):
    code = ""
    jsonData = json.load(open(jsonFile, "r"))
    if "testgen" not in jsonData:
        raise KeyError("A top-level key 'testgen' is required in the JSON.")
    jsonData = jsonData["testgen"]
    
    for m in create_makers(jsonData, {}).itervalues():
        code += m + "\n"
   
    product.varCounter = 0
    search_space_code = ["type search_space = {"]
    to_list_code = [ "let search_space_to_list x = [" ] # TODO replace x

    for ss, tl in product(jsonData):
        if ss != []:
            search_space_code.append(";".join(ss) + ";")
        to_list_code.append(tl + ";")

    search_space_code.append("};;\n\n")
    to_list_code.append("];;\n\n")

    code += "\n"
    code += "\n    ".join(search_space_code)
    code += "\n"
    code += "\n    ".join(to_list_code)

    print header
    print code
    print footer.format(hashlib.md5(code).hexdigest())


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Imandra test-generator-generator')
    parser.add_argument('jobFile', nargs=1)
    args = parser.parse_args()
    generate_code(args.jobFile[0])    
