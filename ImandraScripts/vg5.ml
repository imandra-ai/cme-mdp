(** **************************************************************************************** *)
(** 5: The book must be up-to-date no matter if A or B channel is behind or completely empty *)
(** **************************************************************************************** *)
(** The scenarios should be the following:

        If channel A (inc) is empty or next message there is behind last processed (of the state)
        then one_step correctly processes channel B (if there's a valid message there)

        and vice versa, if channel B (incremental) is empty or next message is behind last 
	processed... 
*)

let set_next_message(s, msg, channel) =
    let packet = {
        packet_seq_num = s.channels.last_seq_processed + 1; 
        packet_messages = [msg];
        packet_channel  = channel
    } in { s with channels = {s.channels with unprocessed_packets = [packet] } }
;; 

theorem[rw] process_msg_recovery_channel_invariant (s, m) =
     let sA = set_next_message( s, m, Ch_Ref_A ) in
     let sB = set_next_message( s, m, Ch_Ref_B ) in
     process_msg_recovery (sA, m, Ch_Ref_A) = process_msg_recovery (sA, m, Ch_Ref_B)
;;

:disable process_msg_recovery

:break

theorem ref(s, ref_msg) =
    let packetA = {
        packet_seq_num = 1; 
        packet_messages = [RefreshMessage ref_msg];
        packet_channel  = Ch_Ref_A
    } in 
    let packetB = { packetA with packet_channel = Ch_Ref_B } in
    let sA = { s with channels = {s.channels with unprocessed_packets = [packetA] } } in 
    let sB = { s with channels = {s.channels with unprocessed_packets = [packetB] } } in 
    ((one_step(sA)).channels.last_seq_processed) = ( (one_step(sB)).channels.last_seq_processed )
;;


verify correct_process_a (s, next_a : feed_state * ref_packet) = (
	next_a.rp_msg.rm_security_id = s.sec_id &&
	correct_level (next_a.rp_msg, s.books.book_depth) && 
	s.feed_status = Normal &&
	s.channels.ref_a.r_unproc_packets = [next_a] &&
	s.channels.ref_b.r_unproc_packets = [] &&
	s.channels.snap_a.s_unproc_packets = [] && 
	s.channels.snap_b.s_unproc_packets = [] && 
	msg_correct_seq (next_a.rp_msg, s.channels) 
	)
	==> ((one_step s).channels.last_seq_processed = next_a.rp_msg.rm_rep_seq_num)
;;

verify correct_process_b (s, next_b : feed_state * ref_packet) = (
	next_b.rp_msg.rm_security_id = s.sec_id &&
	correct_level (next_b.rp_msg, s.books.book_depth) && 
	s.feed_status = Normal &&
	s.channels.ref_a.r_unproc_packets = [] &&
	s.channels.ref_b.r_unproc_packets = [next_b] &&
	s.channels.snap_a.s_unproc_packets = [] && 
	s.channels.snap_b.s_unproc_packets = [] && 
	msg_correct_seq (next_b.rp_msg, s.channels) 
	)
	==> ((one_step s).channels.last_seq_processed = next_b.rp_msg.rm_rep_seq_num)
;;

verify correct_process_snap_a (s, snap_a : feed_state * snap_packet) = 
	(
	snap_a.sp_snap.sm_security_id = s.sec_id && 
	s.feed_status = InRecovery && 
	s.channels.ref_a.r_unproc_packets = [] &&
	s.channels.ref_b.r_unproc_packets = [] &&
	s.channels.snap_a.s_unproc_packets = [snap_a] &&
	s.channels.snap_b.s_unproc_packets = [] && 
	snap_a.sp_snap.sm_last_msg_seq_num_processed > s.channels.last_seq_processed &&
	s.channels.cache = [] &&
	s.channels.last_snapshot = None
	 )
	==> 
	( ((one_step s).channels.last_seq_processed) = snap_a.sp_snap.sm_last_msg_seq_num_processed )
;; 

verify correct_process_snap_b (s, snap_b : feed_state * snap_packet) = 
	(
	snap_b.sp_snap.sm_security_id = s.sec_id && 
	s.feed_status = InRecovery && 
	s.channels.ref_a.r_unproc_packets = [] &&
	s.channels.ref_b.r_unproc_packets = [] &&
	s.channels.snap_a.s_unproc_packets = [snap_b] &&
	s.channels.snap_b.s_unproc_packets = [] && 
	snap_b.sp_snap.sm_last_msg_seq_num_processed > s.channels.last_seq_processed &&
	s.channels.cache = [] &&
	s.channels.last_snapshot = None
	 )
	==> 
	( ((one_step s).channels.last_seq_processed) = snap_b.sp_snap.sm_last_msg_seq_num_processed )
;; 
