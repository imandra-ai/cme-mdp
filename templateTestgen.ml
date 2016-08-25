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


type search_space = { {% for n,e in events %}
    {% if e.type 
        %} x{{n}} : {{e.type}}; {% 
    endif %}{% endfor %}
};;

let search_space_to_list x = [{% for n,e in events %}
    {% for c in e.constructors %}({{c}}{% endfor %} {% 
        if e.type %}x.x{{n}}{% endif %}{% for c in e.constructors %}){% endfor %};{% 
    endfor %}
];;

let run_all m = 
    let empty_state = Some {
        exchange_state = init_ex_state;
        network_state = empty_network_state  
    } in
    run ( empty_state, search_space_to_list m ) 
;;


let valid_all m = 
    let empty_state = Some {
        exchange_state = init_ex_state;
        network_state = empty_network_state  
    } in
    valid ( empty_state, search_space_to_list m ) 
;;


:shadow off
let n = ref 0;;
let write_jsons m =
    let empty_state = Some {
        exchange_state = init_ex_state;
        network_state = empty_network_state  
    } in
    let final_state = run ( empty_state, search_space_to_list m ) in
    match final_state with 
    | None -> " **** Ignoring empty test case ***** " |> print_string
    | Some final_state ->
    let packets = final_state.network_state.outgoing @ final_state.network_state.incoming in
    let () = n := !n + 1 in
    packets |> packets_to_json
            (*|> Yojson.Basic.pretty_to_string |> print_string *)
            |> Yojson.Basic.to_file (Printf.sprintf "{{outdir}}/test_%d.json" !n) 
;;
:shadow on
:adts on

:max_region_time 120
:testgen run_all assuming valid_all with_code write_jsons 


