import json
import itertools

# This makes "let make_type" fuction declarations -- those are very useful 
# to in reducing the size fo the generated code. Also they oranize the "combinatorial"
# and the "seach space" declaraions into subsets going a 
# The code itself is placed in the "recordMakers" dictionary.
def  create_makers(jsonData, vsname="x"):
    recordMakers = {}
    for entry in jsonData:
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

# Simultanteously generates the "search_space" type declaration  
# and a "list of lists" for the cartesian-product-expanded entries in 
# the "search_space_to_list" function 
def create_search_space(jsonData, ssname="x"):
    n = 0
    search_space_code = []
    to_list_entries = []

    search_space_code.append("type search_space = {")

    for entry in jsonData:
        if "records" not in entry:
            cs = entry["constructors"]
            to_list_entries.append(["(".join(cs) + ")"*(len(cs)-1)])        
            continue
        variants = []
        for record in entry["records"]:
            if "variants" in record:
                variants.append(map(str, record["variants"]))

        to_list_entry = []
        for case in itertools.product(*variants):
            mkparams = list(case)
            for record in entry["records"]:
                if "variants" not in record:
                    n += 1
                    ss_record = record["record"] + "_" + str(n)
                    search_space_code.append( ss_record + ":" + record["type"] + ";")
                    mkparams.append(ssname + "." + ss_record)

            constr, closing = "",""
            for c in entry["constructors"] : 
                constr  = constr + c + "("
                closing = closing + ")" 
            constr = constr + "make_" + entry["type"] + "("
            closing = closing + ")" 

            to_list_entry.append( constr + ",".join(mkparams) + closing )
        to_list_entries.append(to_list_entry)

    search_space_code.append("}")
    search_space_code = "\n   ".join(search_space_code)
    return to_list_entries, search_space_code 

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
let empty_state = Some {
    exchange_state = init_ex_state;
    network_state = empty_network_state  
};;

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
            |> Yojson.Basic.to_file (Printf.sprintf "generated/test_%d.json" !n) 
;;
:shadow on
:adts on

:max_region_time 120
:testgen run_all assuming valid_all with_code write_jsons 
'''


def generate_code(jsonFile):
    code = ""
    jsonDict = json.load(open("test.json", "r"))
    jsonData = jsonDict["testgen"]
    
    for m in create_makers(jsonData).itervalues():
        code += m + "\n"
    
    expanded_entries, search_space_code = create_search_space(jsonData)
    code += search_space_code + ";;\n\n"
    
    code += "let search_space_to_list x = [\n    "
    code += ";\n    ".join([";\n    ".join(e) for e in expanded_entries])
    code += "\n];;\n\n"
    print header
    print code
    print footer

generate_code("")    
