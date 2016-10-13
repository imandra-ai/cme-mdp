:load Model/CME_Testgen_Template.ml

type search_space = {
    x0: action;
    x1: action;
    x2: action;
    x3: action;
};;
let search_space_to_list action = [
    action.x0;
    action.x1;
    action.x2;
    action.x3;
];;

let   run_all actions = run   ( Some init_ex_state, search_space_to_list actions);;
let valid_all actions = valid ( Some init_ex_state, search_space_to_list actions);;

:shadow off
let n = ref 0;;
let write_jsons actions =
    let final_state = run_all actions in
    match final_state with 
    | None -> " **** Ignoring empty test case ***** " |> print_string
    | Some final_state ->
    let () = n := !n + 1 in
    let filename = Printf.sprintf "test_%d.ml" !n in
    final_state |> exchange_state_to_ocaml
                |> output_string (open_out filename) 
;;
:shadow on

:adts on
:max_region_time 60
:testgen run_all assuming valid_all with_code write_jsons 
