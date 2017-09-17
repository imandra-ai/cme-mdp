(** **************************************************************************************** *)
(** 3: If the reset message is sent, the book must be empty until naturally refreshed        *)
(** **************************************************************************************** *)

(** Prove that on MsgReset the book is transitioning to InRecovery *)
verify in_recovery (s) =
  let message = get_next_message (s) in
  match message with
    Some (RefreshMessage ref_message) ->
    (is_msg_reset (ref_message) && 
     s.channels.cache = [] &&
     s.channels.last_seq_processed > 0 &&
     s.feed_status <> InRecovery && 
     is_msg_relevant (ref_message, s) && 
     msg_correct_seq (ref_message, s.channels) &&
	   ref_message.rm_security_id = s.feed_sec_id)
    ==>
      ((one_step s).feed_status = InRecovery)
  | _ -> true
;;

