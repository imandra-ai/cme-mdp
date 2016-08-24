:load Model/CME_Types.ml
:load Model/CME_Exchange.ml

(* 
  @meta[measure : run]
    let measure_run (s, actions) = List.length actions
  @end
*)

let rec run (s, acts) = match acts with
   | [] -> s 
   | act :: acts -> 
      let es = process_exchange_trans (s, act) in 
      run (es, acts) 
;;
:stage run

(* @meta[measure : valid]
    let measure_valid (s, actions) = List.length actions
    @end
*)

let rec valid (s, acts) = match acts with
    | [] -> true 
	| act :: acts -> 
        is_exchange_trans_valid (s, act) && (
        let es = process_exchange_trans (s, act) in 
        valid ( es, acts) )
;;

type search_space = {
    m1  : exchange_transition;
    m2  : exchange_transition;
    m3  : exchange_transition;
    m4  : exchange_transition;
    m5  : exchange_transition;
    m6  : exchange_transition;
    m7  : exchange_transition;
    m8  : exchange_transition; 
};;

let search_space_to_list m = [m.m1;m.m2;m.m3;m.m4;m.m5;m.m6;m.m7;m.m8];;

let valid_all (inc_msg_queue, m) = 
    valid( { init_ex_state with inc_msg_queue = inc_msg_queue }, 
           search_space_to_list m ) 
;;

let run_all (inc_msg_queue, m) = 
    run( { init_ex_state with inc_msg_queue = inc_msg_queue }, 
         search_space_to_list m ) 
;;

:testgen run_all assuming valid_all
