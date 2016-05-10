(** ****************************************************************************** *)
(**
    If the packet is missed, the book must be empty until recovered
     (via snapshot or natural refresh)
*)

verify empty_packet_forces_recovery (s) =
    let new_p = get_next_packet (s.channels) in
    match new_p.p with
      IncRefreshPacket sp ->
      ( s.feed_status = Normal && 
        correct_level (sp.rp_msg, s.books.book_depth) &&
        sp.rp_msg.rm_rep_seq_num > (s.channels.last_seq_processed + 1) && 
        sp.rp_msg.rm_security_id = s.sec_id)
      ==>
        ((one_step s).feed_status = InRecovery)
    | _ -> true
;;

(* Second part of VG4 *)
:disable process_msg_recovery is_cache_valid_since_seq_num

(* PROVED *)
verify good_recovery_light (s) =
  (
    s.feed_status = InRecovery && 
    s.channels.last_seq_processed > 0 &&
    let s' = process_msg_recovery s in
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

verify good_recovery_proc_msg_recovery (s) =
  (s.feed_status = InRecovery
   && is_cache_valid_since_seq_num (s.channels.cache, s.channels.last_seq_processed, s.books.book_depth)
   && match (get_next_packet (s.channels)).p with
        (IncRefreshPacket ip) ->
        ip.rp_msg.rm_rep_seq_num = s.channels.last_seq_processed + 1
      | _ -> false)
  ==>
    let s' = process_msg_recovery s in
    is_cache_valid_since_seq_num (s'.channels.cache, s'.channels.last_seq_processed, s.books.book_depth)
;;

:disable process_msg_recovery

verify good_recovery_no_new_snap (s) =
  (s.feed_status = InRecovery
   && is_cache_valid_since_seq_num (s.channels.cache, s.channels.last_seq_processed, s.books.book_depth)
   && s.channels.snap_a.s_unproc_packets = []
   && s.channels.snap_b.s_unproc_packets = []
   && s.channels.ref_a.r_unproc_packets = []
   && s.channels.ref_b.r_unproc_packets = [])
    ==>
    ((one_step s).feed_status = Normal)
;;



