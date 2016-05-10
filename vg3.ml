(** **************************************************************************************** *)
(** 3: If the reset message is sent, the book must be empty until naturally refreshed        *)
(** **************************************************************************************** *)

(** Prove that on MsgReset the book is transitioning to InRecovery *)
verify in_recovery (s) =
  let new_p = get_next_packet (s.channels) in
  match new_p.p with
    IncRefreshPacket sp ->
    (is_msg_reset (sp.rp_msg) && 
     s.channels.cache = [] &&
     s.channels.last_seq_processed > 0 &&
     s.feed_status <> InRecovery && 
     is_msg_relevant (sp.rp_msg, s) && 
     msg_correct_seq (sp.rp_msg, s.channels) &&
	   sp.rp_msg.rm_security_id = s.sec_id)

    ==>
      ((one_step s).feed_status = InRecovery)
  | _ -> true
;;

