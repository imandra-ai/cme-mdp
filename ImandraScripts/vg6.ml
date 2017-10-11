(** **************************************************************************************** *)
(** 6: If the instrument is not in snapshot channel (illiquid instr case), the book should be
                populated from incremental channel *)
(** **************************************************************************************** *)

let both_illiquid (s) =
    s.channels.cycle_hist_a.liq = Illiquid &&  
    s.channels.cycle_hist_b.liq = Illiquid 
;;

theorem[rw] illiquid_attempt_recovery (s) =
    ( both_illiquid s )
    ==>
    ( (attempt_recovery s).feed_status = Normal )
;;

:disable attempt_recovery

theorem[rw] illiquid_constant(s,m,c) =
    both_illiquid(s) 
    ==>
    both_illiquid(process_msg_recovery(s,m,c))
;;

:disable process_msg_recovery

theorem illiquid_nat_refresh (s) =
   ( both_illiquid s &&
     s.feed_status = InRecovery &&
     get_next_message s <> None )
    ==>
   ((one_step s).feed_status = Normal)
;;
