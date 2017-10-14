(* Proving constancy of record fields *)

(* PROVEN *)
theorem[rw] apply_update_depth_constant (bs, cache) = 
    (apply_update_packets(bs, cache)).book_depth = bs.book_depth;;

(* PROVEN *)
theorem[rw] apply_cache_depth_constant (bs, cs) = 
    (apply_cache(bs, cs)).book_depth = bs.book_depth;;

(* PROVEN *)
theorem[rw] apply_snapshot_depth_constant (bs, cs) =
   (apply_snapshot(bs, cs)).book_depth = bs.book_depth;;

(* PROVEN *)
theorem[rw] process_md_update_depth_constant (bs, msg) =
   (process_md_update_action (bs, msg)).book_depth = bs.book_depth;;

(* PROVEN *)
theorem[rw] process_msg_normal_depth_constant (s, msg) =
    (process_msg_normal (s, msg)).books.book_depth = s.books.book_depth;;

(* PROVEN *)
theorem[rw] attempt_recovery_depth_constant(s) =
    (attempt_recovery s).books.book_depth = s.books.book_depth;;

(* PROVEN *)
theorem[rw] process_msg_recovery_depth_constant (s, msg, channels) =
    (process_msg_recovery (s, msg, channels)).books.book_depth = s.books.book_depth;;

(* PROVEN *)
theorem[rw] add_int_message_depth_constant (s, imsg) =
    (add_int_message (s, imsg)).books.book_depth = s.books.book_depth;;

theorem[rw] process_rec_snapshot_depth_constant (s, msg, channel) =
    (process_rec_snapshot (s, msg, channel)).books.book_depth = s.books.book_depth;;

(*
theorem[rw] one_step_depth_constant (s) =
   (one_step s).books.book_depth = s.books.book_depth;;
*)

