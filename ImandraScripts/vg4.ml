(** ****************************************************************************** *)
(**
    If the packet is missed, the book must be empty until recovered
     (via snapshot or natural refresh)
*)

(* Part1: a gap in refresh messages changes status to recovery *)
verify packet_gap_forces_recovery (s) =
    let message = get_next_message (s) in
    match message with
      Some(RefreshMessage ref_message) ->
      ( s.feed_status = Normal && 
        correct_level (ref_message, s.books.book_depth) &&
        ref_message.rm_rep_seq_num > (s.channels.last_seq_processed + 1) && 
        ref_message.rm_security_id = s.feed_sec_id)
      ==>
        ((one_step s).feed_status = InRecovery)
    | _ -> true
;;


(* Part2: if recovery/snapshot message makes cache valid -- transition to normal state *)

(* - hypothese for cache validity *)
let is_cache_valid_last_msg  ( s : feed_state ) =
    let cache = s.channels.cache in
    let last_seq = s.channels.last_seq_processed in
    is_cache_valid_since_seq_num (cache, last_seq, s.books.book_depth)
;;

let is_cache_valid_last_snap ( s : feed_state ) =
    let cache = s.channels.cache in
    match s.channels.last_snapshot with None -> false | Some snap -> 
    let last_seq = snap.snap_last_msg_seq_num_processed in
    is_cache_valid_since_seq_num (cache, last_seq, s.books.book_depth) 
;;

(* Proving that attempt_recovery gets to Normal *)

theorem[rw] attempt_recovery_msg (s) =
    ( is_cache_valid_last_snap s )
     ==>
    (attempt_recovery s).feed_status = Normal
;;

theorem[rw] attempt_recovery_snap (s) =
    ( is_cache_valid_last_msg s )
     ==>
    (attempt_recovery s).feed_status = Normal
;;

:disable attempt_recovery
:disable is_cache_valid_since_seq_num

let is_cache_valid_after_processing s =
    match s.channels.unprocessed_packets with [] -> false | packet::rest_packets ->
    match packet.packet_messages with [] -> false | message::rest_messages ->
    let s' = process_msg_recovery ( s, message, packet.packet_channel ) in
    (is_cache_valid_last_msg s' || is_cache_valid_last_snap s')
;;

:disable process_msg_recovery

(* PROVEN *)
theorem recovery_both (s) =
    (  is_cache_valid_after_processing s 
      &&  s.feed_status = InRecovery 
    ) ==>
    (one_step s).feed_status = Normal
;;
