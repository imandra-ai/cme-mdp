(** **************************************************************************************** *)
(** 6: If the instrument is not in snapshot channel (illiquid instr case), the book should be
                populated from incremental channel *)
(** **************************************************************************************** *)

verify illiquid_nat_refresh (s) =
 (s.channels.cycle_hist_a.liq = Illiquid
  && s.channels.cycle_hist_b.liq = Illiquid
  && s.feed_status = InRecovery)
 ==>
   ((one_step s).feed_status = Normal)
;;
