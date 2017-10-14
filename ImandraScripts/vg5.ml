(** **************************************************************************************** *)
(** 5: The book must be up-to-date no matter if A or B channel is behind or completely empty *)
(** **************************************************************************************** *)
(** The scenarios should be the following:

        If channel A (inc) is empty or next message there is behind last processed (of the state)
        then one_step correctly processes channel B (if there's a valid message there)

        and vice versa, if channel B (incremental) is empty or next message is behind last 
	processed... 
*)

let states_eq(s1, s2) =
    ( s1.feed_status = s2.feed_status ) &&
    ( s1.channels.last_seq_processed = s2.channels.last_seq_processed ) &&
    ( s1.books = s2.books ) 
;;

let set_next_message(s, msg, channel) =
    let packet = {
        packet_seq_num = s.channels.last_seq_processed + 1; 
        packet_messages = [msg];
        packet_channel  = channel
    } in { s with channels = {s.channels with unprocessed_packets = [packet] } }
;; 

(* PROVEN *)
theorem[rw] process_msg_recovery_ref_channel_invariant (s, m) =
     let sA = set_next_message( s, m, Ch_Ref_A ) in
     let sB = set_next_message( s, m, Ch_Ref_B ) in
     let sA = process_msg_recovery (sA, m, Ch_Ref_A) in  
     let sB = process_msg_recovery (sB, m, Ch_Ref_B) in
     states_eq(sA, sB)
;;

(* PROVEN *)
theorem[rw] process_msg_recovery_snap_channel_invariant (s, m) =
     let sA = set_next_message( s, m, Ch_Snap_A ) in
     let sB = set_next_message( s, m, Ch_Snap_B ) in
     let sA = process_msg_recovery (sA, m, Ch_Snap_A) in  
     let sB = process_msg_recovery (sB, m, Ch_Snap_B) in
     states_eq(sA, sB)
;;

:disable process_msg_recovery

(* PROVEN *)
theorem[rw] process_msg_normal_ref_channel_invariant (s, m) =
    let sA = set_next_message( s, m, Ch_Ref_A ) in
    let sB = set_next_message( s, m, Ch_Ref_B ) in
    let sA = process_msg_normal (sA, m) in  
    let sB = process_msg_normal (sB, m) in
    states_eq(sA, sB)
;;

(* PROVEN *)
theorem[rw] process_msg_normal_snap_channel_invariant (s, m) =
    let sA = set_next_message( s, m, Ch_Snap_A ) in
    let sB = set_next_message( s, m, Ch_Snap_B ) in
    let sA = process_msg_normal (sA, m) in  
    let sB = process_msg_normal (sB, m) in
    states_eq(sA, sB)
;;

:disable process_msg_normal

(* PROVEN *)
verify one_step_A_B_invariant(s, m) = 
    let sA = set_next_message( s, m, Ch_Ref_A ) in
    let sB = set_next_message( s, m, Ch_Ref_B ) in
    states_eq(one_step sA, one_step sB)
;;
 

