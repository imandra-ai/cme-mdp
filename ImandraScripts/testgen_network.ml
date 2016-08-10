#use "topfind";;
#require "yojson";;

:load Model/CME_Network.ml

:adts
:p (in-theory (enable IML-ADT-EXECUTABLE-COUNTERPARTS-THEORY))

let rec mklist (n,c) = if n > 0 then c::mklist(n-1, c+1) else [];;

let start_network_state = {
    incoming = mklist(10,0);
    outgoing = [];
    cache    = [];
};; 

type n_eight = {
    n1 : net_effect;
    n2 : net_effect;
    n3 : net_effect;
    n4 : net_effect;
    n5 : net_effect;
    n6 : net_effect;
    n7 : net_effect;
    n8 : net_effect;
};;

let eight ( n : n_eight ) = true ;;

let valid_effect_8_s ( s1, s2, s3, s4, s5, s6, s7, s8, n ) = 
       is_net_effect_valid (s1, n.n1)
    && is_net_effect_valid (s2, n.n2)
    && is_net_effect_valid (s3, n.n3)
    && is_net_effect_valid (s4, n.n4)
    && is_net_effect_valid (s5, n.n5)
    && is_net_effect_valid (s6, n.n6)
    && is_net_effect_valid (s7, n.n7)
    && is_net_effect_valid (s8, n.n8)
;;

(** Are these effects valid? *)
let valid_effect_8 ( n : n_eight ) = 
    let s1  = start_network_state in 
    let s2  = process_net_effect (s1, n.n1) in 
    let s3  = process_net_effect (s2, n.n2) in 
    let s4  = process_net_effect (s3, n.n3) in 
    let s5  = process_net_effect (s4, n.n4) in 
    let s6  = process_net_effect (s5, n.n5) in 
    let s7  = process_net_effect (s6, n.n6) in 
    let s8  = process_net_effect (s7, n.n7) in 
    valid_effect_8_s ( s1, s2, s3, s4, s5, s6, s7, s8, n ) 
;;

:shadow off
let i = ref 0;;

let write_jsons n =
    let net_state = simulate_network (start_network_state , [ n.n1; n.n2; n.n3; n.n4; n.n5; n.n6; n.n7; n.n8 ]) in
    let () = i := !i + 1 in
    net_state.outgoing |> List.rev 
                       |> List.map (fun x -> `Int x)
                       |> (fun x -> `Assoc[ ( "Permutation", `List x ) ] )
                       (* |> Yojson.Basic.pretty_to_string |> print_string *)
                       |> Yojson.Basic.to_file (Printf.sprintf "network_cases/test_%d.json" !i)
;;
 
:shadow on

:max_region_depth 8
:testgen eight assuming valid_effect_8 with_code write_jsons
 


