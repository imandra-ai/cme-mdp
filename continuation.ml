#use "topfind";;
#require "yojson";;

:load Model/CME_Types.ml
:load Model/CME_Exchange.ml
:load Model/CME_Network.ml
:load_ocaml Printers/CME_json.ml
:load_ocaml Printers/CME_Exchange_json.ml

type action =
    | BookAction     of book_transition
    | ExchangeAction of exchange_transition
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
        let es = process_book_trans (s, act) in 
        run (Some es, acts)  
    | Some s, ExchangeAction act :: acts -> 
        let es = process_exchange_trans (s, act) in 
        run (Some es, acts)
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
        is_book_trans_valid (s, act) && (
        let es = process_book_trans (s, act) in 
        valid (Some es, acts) )
    | Some s, ExchangeAction act :: acts -> 
        is_exchange_trans_valid (s, act) && (
        let es = process_exchange_trans (s, act) in 
        valid (Some es, acts) )
;;


:load state.ml

type search_space = {
    b1 : book_transition;
    b2 : book_transition;
    b3 : book_transition;
    e1 : exchange_transition;
    e2 : exchange_transition;
    e3 : exchange_transition;
};;

let search_space_to_list x = [
    BookAction x.b1;
    BookAction x.b2;
    BookAction x.b3;
    ExchangeAction x.e1;
    ExchangeAction x.e2;
    ExchangeAction x.e3;
];;

let run_all m = run ( Some starting_state, search_space_to_list m ) ;;

let valid_all m = valid ( Some starting_state, search_space_to_list m ) ;;

:shadow off
let n = ref 0;;
let write_jsons m =
    let final_state = run ( Some starting_state, search_space_to_list m ) in
    match final_state with 
    | None -> " **** Ignoring empty test case ***** " |> print_string
    | Some final_state ->
    let () = n := !n + 1 in
    final_state |> exchange_state_to_json
                |> Yojson.Basic.to_file (Printf.sprintf "generatedNext/%s_%d.json" fcode (!n) )  
;;
:shadow on
:adts on

:max_region_time 120
:testgen run_all assuming valid_all with_code write_jsons 

