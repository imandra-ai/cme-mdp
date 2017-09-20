(** ****************************************************************************** *)
(**
    If the packet is missed, the book must be empty until recovered
     (via snapshot or natural refresh)
*)

verify empty_packet_forces_recovery (s) =
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

(* Second part of VG4 *)
:disable process_msg_recovery is_cache_valid_since_seq_num

let get_next_channel s =
    match s.channels.unprocessed_packets with [] -> None 
    | current_packet::rest_packets -> Some ( current_packet.packet_channel )
;;

(* PROVED *)
verify good_recovery_light (s) =
  match get_next_channel (s) with None -> true | Some c ->
  match get_next_message (s) with None -> true | Some m ->
  (
    s.feed_status = InRecovery && 
    s.channels.last_seq_processed > 0 &&
    let s' = process_msg_recovery (s,m,c) in
      is_cache_valid_since_seq_num (s'.channels.cache, s'.channels.last_seq_processed, s.books.book_depth))
  ==>
    ((one_step s).feed_status = Normal)
;;


let rec final_rep_seq_num_in_lst (ms) =
  match ms with
    [] -> 0
  | [m] -> m.rm_rep_seq_num
  | m :: ms -> final_rep_seq_num_in_lst ms
;;

(* ! Counterexample *)
verify good_recovery_proc_msg_recovery (s) =
  (s.feed_status = InRecovery
   && is_cache_valid_since_seq_num (s.channels.cache, s.channels.last_seq_processed, s.books.book_depth)
   && match (get_next_message (s)) with
        Some( RefreshMessage ref_message) ->
        ref_message.rm_rep_seq_num = s.channels.last_seq_processed + 1
      | _ -> false)
  ==>
    let s' = one_step s in
    is_cache_valid_since_seq_num (s'.channels.cache, s'.channels.last_seq_processed, s.books.book_depth)
;;

:disable process_msg_recovery

(* ! Counterexample *)
verify good_recovery_no_new_snap (s) =
  (s.feed_status = InRecovery
   && is_cache_valid_since_seq_num (s.channels.cache, s.channels.last_seq_processed, s.books.book_depth)
   && s.channels.unprocessed_packets = [])
    ==>
    ((one_step s).feed_status = Normal)
;;



