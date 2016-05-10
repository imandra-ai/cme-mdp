(** **************************************************************************************** *)
(** 5: The book must be up-to-date no matter if A or B channel is behind or completely empty *)
(** **************************************************************************************** *)
(** The scenarios should be the following:

        If channel A (inc) is empty or next message there is behind last processed (of the state)
        then one_step correctly processes channel B (if there's a valid message there)

        and vice versa, if channel B (incremental) is empty or next message is behind last 
	processed... 
*)

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
