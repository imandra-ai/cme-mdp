(* VG1 - CME book sorting *)

(** ************************************************************* *)
(** 1: Under no circumstances the book is unsorted                *)
(** ************************************************************* *)

(** Check that after every update to the book, the book is sorted *)

(* Sanity check: order_higher_ranked is transitive                *)
(* PROVED *)

verify ohr_trans (s, o1, o2, o3) =
  (order_higher_ranked (s, o1, o2)
   && order_higher_ranked (s, o2, o3))
  ==>
  (order_higher_ranked (s, o1, o3))
;;

(* Sanity check: order_higher_ranked is reflexive *)
(* PROVED *)

verify ohr_refl (s, o1) =
  order_higher_ranked (s, o1, o1);;

(* Sanity check: order_higher_ranked is antisymmetric *)
(* REFUTED - this doesn't hold!

verify ohr_anti_symm (s, o1, o2) =
  (order_higher_ranked (s, o1, o2)
   && order_higher_ranked (s, o2, o1))
  ==>
    (o1 = o2)
;;

 *)

(* What it means to be sorted *)

let rec is_side_sorted_raw (side, ls) =
  match ls with
    [] -> true
  | [a] -> true
  | a :: b :: rst -> order_higher_ranked (side, a, b)
                     && is_side_sorted_raw (side, b :: rst)
;;

let is_book_sorted (b : book) =
  is_side_sorted_raw (OrdBuy, b.buys)
  && is_side_sorted_raw (OrdSell, b.sells)
;;

theorem[rw] is_sorted_maintained_by_insert (l, side, ls) =
  (is_side_sorted_raw (side, ls))
  ==>
    (is_side_sorted_raw (side, insert_order (l, side, ls)))
;;

:disable order_higher_ranked

theorem[rw] is_sorted_idempotent_raw (side, ls) =
  (is_side_sorted_raw (side, ls))
  ==>
    (sort_side (ls, side) = ls);;

theorem[rw] sort_side_is_sorted_raw (side, ls) =
  (is_side_sorted_raw (side, sort_side (ls, side)));;

:enable order_higher_ranked

(* Sorted book is sorted! *)

theorem[rw] sorted_book_is_sorted (b : book) =
  is_book_sorted (sort_book b)
;;

(* Reset_books sorts the books, trivially *)

:p (in-theory (enable |NoLevel|))

theorem[rw] empty_order_levels_empty (n) =
  (n > 0) ==> (List.hd (empty_order_levels n) = NoLevel)
;;

theorem[rw] empty_order_levels_empty' (n) =
  (empty_order_levels n <> [])
  ==>
    (List.hd (empty_order_levels n) = NoLevel)
;;

:p (in-theory (disable |NoLevel|))

theorem[rw] prepending_NoLevel_is_sorted_ALL (ls, side) =
  (is_side_sorted_raw (side, ls)
   && List.hd ls = NoLevel)
  ==>
    (is_side_sorted_raw (side, NoLevel :: ls))
;;

theorem[rw] eolsB_IH_ALL (n, side) =
  (is_side_sorted_raw (side, empty_order_levels n))
  ==>
    (is_side_sorted_raw (side, NoLevel :: empty_order_levels n))
;;

theorem[rw] empty_order_levels_sorted_ALL (n, side) =
  is_side_sorted_raw (side, empty_order_levels n)
;;

(* Empty book sorts! *)

theorem[rw] empty_book_sorted (n) =
  is_book_sorted (empty_book n);;

:disable empty_book
:disable is_book_sorted

theorem[rw] reset_books_sorted_multi (bs : books) =
  let bs' = reset_books bs in
  is_book_sorted (bs'.multi)
;;

theorem[rw] reset_books_sorted_implied (bs : books) =
  let bs' = reset_books bs in
  is_book_sorted (bs'.implied)
;;

theorem[rw] reset_books_sorted_combined_ (bs : books) =
  let bs' = reset_books bs in
  is_book_sorted (bs'.combined)
;;

theorem[rw] reset_books_status_empty (bs : books) =
  (reset_books bs).b_status = Empty
;;

let books_sorted (b : books) =
  is_book_sorted b.implied
  && is_book_sorted b.multi
  && is_book_sorted b.combined
;;

:enable reset_books books_sorted

theorem[rw] reset_books_sorted_all (bs : books) =
  books_sorted (reset_books bs);;

:disable reset_books

(* trim_side respects sorting *)

(* A simpler version of trim_side, that we prove equivalent to the original
    w.r.t. a bit of numerical fiddling (num_levels - curr_level + 1). *)

let rec trim_side' (ords, num_levels : order_level list * int) =
  if num_levels <= 0
  then []
  else match ords with
         [] -> []
       | o :: os -> o :: (trim_side' (os, num_levels - 1))
;;

theorem[rw] trim_side_eq_trim_side' (ords, num_levels, curr_level) =
  (trim_side (ords, num_levels, curr_level) = trim_side' (ords, num_levels - curr_level + 1))
;;

:disable order_higher_ranked
:disable trim_side trim_side'
:enable trim_side'

(* List.tl of a sorted list is sorted *)

theorem[rw] tl_of_sorted_lst_is_sorted (side, ls) =
  (is_side_sorted_raw (side, ls))
  ==>
    (is_side_sorted_raw (side, List.tl ls))
;;

(* Axiom: trim_side' of a sorted side is still sorted *)

axiom[rw] trim_side'_respects_sorting (side, ls, n) =
  (is_side_sorted_raw (side, ls))
  ==>
    (is_side_sorted_raw (side, trim_side' (ls, n)))
;;

:disable is_side_sorted_raw
:disable trim_side'

theorem[rw] trim_side_respects_sorting (side, ls, n, m) =
  (is_side_sorted_raw (side, ls))
  ==>
    (is_side_sorted_raw (side, trim_side (ls, n, m)))
;;

theorem[rw] trim_side_rw (ords, num_levels) =
  trim_side (ords, num_levels, 1) = trim_side' (ords, num_levels)
;;

:enable is_book_sorted

theorem[rw] is_book_sorted_implied_by (buys, sells) =
  (is_side_sorted_raw (OrdBuy, buys)
   && is_side_sorted_raw (OrdSell, sells))
  ==>
    (is_book_sorted { buys = buys;
                      sells = sells })
;;

theorem[rw] is_book_sorted_implies_sides_sorted (b : book) =
  (is_book_sorted b)
  ==>
    (is_side_sorted_raw (OrdBuy, b.buys)
     && is_side_sorted_raw (OrdSell, b.sells))
;;

:disable is_book_sorted

(* process_msg_recovery: books are not changed *)

verify process_msg_recovery_does_not_change_books (s, next_message, channel_type : feed_state * message * channel_type) =
  let s' = process_msg_recovery (s, next_message, channel_type)  in
  s'.books = s.books
;;

(* process_msg_recovery: sorting invariant *)

:disable process_msg_recovery

theorem[rw] book_always_sorted_process_msg_recovery (s, m, c : feed_state * message * channel_type) =
  (books_sorted s.books)
  ==>
    (books_sorted (process_msg_recovery (s,m,c) ).books)
;;

:disable is_book_sorted
:disable sort_book bk_new bk_change bk_delete

(* clean_multi_depth_book maintains sorting *)

:!p
(defthm book_depth_is_int
        (implies (|books-P| books)
                 (integerp (|books->book_depth| books)))
 :rule-classes (:rewrite :type-prescription))
;;

:enable clean_multi_depth_book
:enable books_sorted

theorem[rw] clean_multi_depth_books_sorts (books) =
  books_sorted (clean_multi_depth_book books)
;;

:disable clean_multi_depth_book
:disable books_sorted

(* Previously was an axiom, now proving ... *)

:enable books_sorted

(* We need:

       (|order_level_LIST-P|
              (|bk_new| (|book->buys| (|books->multi| |books|))
                        (OrdBuy)
                        (|ref_message->rm_price_level| |msg|)
                        (|ref_message->rm_entry_size| |msg|)
                        (|ref_message->rm_entry_px| |msg|)
                        (|Some| (|ref_message->rm_num_orders| |msg|)))))

    need:

      (|order_level_LIST-P| (|book->buys| (|books->multi| |books|)))
      (|side-P| (OrdBuy))
      (INTEGERP (|ref_message->rm_price_level| |msg|))
      (INTEGERP (|ref_message->rm_entry_size| |msg|))
      (INTEGERP (|ref_message->rm_entry_px| |msg|))
      (|opt_int_type-P| (|Some| (|ref_message->rm_num_orders| |msg|)))

    with hyps

      (|books-P| |books|)
      (|ref_message-P| |msg|)


 *)

:!p
(defthm ref_message_typing_1
        (implies (|ref_message-P| |msg|)
                 (INTEGERP (|ref_message->rm_price_level| |msg|))))
;;

:!p
(defthm ref_message_typing_2
        (implies (|ref_message-P| |msg|)
                 (INTEGERP (|ref_message->rm_entry_size| |msg|))))
;;

:!p
(defthm ref_message_typing_3
        (implies (|ref_message-P| |msg|)
                 (INTEGERP (|ref_message->rm_entry_px| |msg|))))
;;

theorem[rw] process_md_update_action_sorted_books (books, msg) =
  (books_sorted books) ==> (books_sorted (process_md_update_action (books, msg)))
;;

:disable process_md_update_action

theorem[rw] book_always_sorted_apply_update_backets (books, packets) =
   (books_sorted books)
  ==>
    (books_sorted (apply_update_packets (books, packets)))
;;

:disable apply_update_packets

theorem[rw] book_always_sorted_apply_cache (books, channels) =
   (books_sorted books)
  ==>
    (books_sorted (apply_cache (books, channels)))
;;

:disable apply_cache

(* process_refresh_action gives sorted snapshots *)

(*
:enable is_book_sorted
:disable is_side_sorted_raw
:disable trim_side'
:enable process_refresh_action

theorem[rw] process_refresh_action_sorted_snapshot_impl_m (books, packet) =
  (is_book_sorted (empty_book books.book_depth))
    ==>
  (is_book_sorted (process_refresh_action (books, packet)).snap_m_book)
;;

theorem[rw] process_refresh_action_sorted_snapshot_impl_i (books, packet) =
  (is_book_sorted (empty_book books.book_depth))
    ==>
  (is_book_sorted (process_refresh_action (books, packet)).snap_i_book)
;;

theorem[rw] empty_book_to_depth_sorted (books) =
  is_book_sorted (empty_book books.book_depth)
;;

:disable process_refresh_action

theorem[rw] process_refresh_action_sorted_snapshot_m (books, packet) =
  (is_book_sorted (process_refresh_action (books, packet)).snap_m_book)
;;

theorem[rw] process_refresh_action_sorted_snapshot_i (books, packet) =
  (is_book_sorted (process_refresh_action (books, packet)).snap_i_book)
;;

:disable is_book_sorted
*)

(* process_msg_recovery maintains sorted snapshots *)

:enable process_msg_recovery

let snapshots_sorted (snap) =
  is_book_sorted snap.snap_m_book
  && is_book_sorted snap.snap_i_book
;;

let snapshots_sorted_opt (snap_opt) =
  match snap_opt with
    None -> true
  | Some snap -> snapshots_sorted snap;;

:!p
  (defthm elim-some-match
                       (equal (equal (|Some| x) (|Some| y))
                              (equal x y)))
;;

:!p
  (defthm state_books_type
          (implies (|feed_state-P| s)
                   (|books-P| (|feed_state->books| s))))
;;

:!p
  (defthm packet_list_car_type
          (implies (|channel_state-P| x)
                   (|packet_LIST-P| (|channel_state->unproc_packets| x))))
;;

:!p
  (defthm packet_list_car_type_2
          (implies (and (|channel_state-P| x)
                          (|channel_state->unproc_packets| x))
                   (|packet-P| (CAR (|channel_state->unproc_packets| x)))))
;;

:!disable comp_packets get_next_channel_packet get_packet_time
  new_snapshot_better snapshots_sorted update_cycle_hist
;;

:disable process_refresh_action

(* PROVED *)

theorem[fc] process_msg_recovery_snapshots_sorted_1_1 (s, snap1, snap2) =
  let channels = s.channels in
  let books = s.books in
  let next_packet = get_next_packet (s.channels) in
  let src = next_packet.source in
  (snapshots_sorted_opt s.channels.last_snapshot)
    ==>
      match next_packet.p with
        NoPacket -> snapshots_sorted_opt ((process_msg_recovery s).channels.last_snapshot)
      | _ -> true
;;

(* PROVED *)

theorem[fc] process_msg_recovery_snapshots_sorted_1_2 (s, snap1, snap2) =
  let channels = s.channels in
  let books = s.books in
  let next_packet = get_next_packet (s.channels) in
  let src = next_packet.source in
  (snapshots_sorted_opt s.channels.last_snapshot)
    ==>
      match next_packet.p with
         | IncRefreshPacket ip ->
            snapshots_sorted_opt ((process_msg_recovery s).channels.last_snapshot)
         | _ -> true
;;

(* So, the only case we need to worry about is when, in process_msg_recovery,
    we've got np = SnapshotPacket sp.
 *)

:enable snapshots_sorted

(* PROVED *)

theorem[fc] process_msg_recovery_snapshots_sorted_1_3 (s, snap1, snap2) =
  let channels = s.channels in
  let books = s.books in
  let next_packet = get_next_packet (s.channels) in
  let src = next_packet.source in
  (snapshots_sorted_opt s.channels.last_snapshot)
    ==>
      match next_packet.p with
        | SnapshotPacket sp ->
          let this_ch = process_snap_ch (get_snap_channel (channels, src)) in
          let channels' = set_snap_channel (channels, this_ch, src) in
          let last_snap = process_refresh_action (books, sp) in
          
          snapshots_sorted (last_snap)
        | _ -> true
;;

(* PROVED *)

theorem[fc] process_msg_recovery_snapshots_sorted_1_4 (s, snap1, snap2) =
  let channels = s.channels in
  let books = s.books in
  let next_packet = get_next_packet (s.channels) in
  let src = next_packet.source in
  (snapshots_sorted_opt s.channels.last_snapshot)
    ==>
      match next_packet.p with
         | SnapshotPacket sp ->
            let this_ch = process_snap_ch (get_snap_channel (channels, src)) in
            let channels' = set_snap_channel (channels, this_ch, src) in
            let last_snap = process_refresh_action (books, sp) in
            let new_snap = (
              if new_snapshot_better (last_snap, channels'.last_snapshot) then
                Some last_snap
              else
                channels'.last_snapshot) in

            snapshots_sorted_opt (new_snap)
        | _ -> true
;;

(* PROVED *)

theorem[rw] process_msg_recovery_snapshots_sorted (s,m,c, snap1, snap2) =
  let s' = process_msg_recovery (s,m,c) in
  let snap_opt = s'.channels.last_snapshot in
  (s.channels.last_snapshot = Some snap1
   && snapshots_sorted snap1
   && snap_opt = Some snap2)
  ==> (snapshots_sorted snap2)
;;

:enable process_msg_recovery

(* An axiom we must prove *)

axiom[rw] process_msg_recovery_snapshots_sorted_opt (s, m, c  : feed_state * message * channel_type) =
  (snapshots_sorted_opt s.channels.last_snapshot)
  ==>
    (snapshots_sorted_opt (process_msg_recovery (s,m,c)).channels.last_snapshot)
;;

:disable process_msg_recovery

(* Now we list snapshots sorting results to attempt_recovery *)

theorem[rw] attempt_recovery_maintains_sorted_books (s) =
  (books_sorted s.books
   && snapshots_sorted_opt s.channels.last_snapshot)
  ==>
    (books_sorted (attempt_recovery s).books)
;;

(* PROVED *)

theorem[fc] books_sorted_fc (b) =
  (books_sorted b)
  ==>
    (is_book_sorted b.implied
     && is_book_sorted b.multi
     && is_book_sorted b.combined)
;;

(* PROVED *)

theorem[rw] books_sorted_rw_1 (b) =
  (books_sorted b)
  ==>
    (is_book_sorted b.multi)
;;

(* PROVED *)

theorem[rw] books_sorted_rw_2 (b) =
  (books_sorted b)
  ==>
    (is_book_sorted b.implied)
;;

(* PROVED *)

theorem[rw] books_sorted_rw_3 (b) =
  (books_sorted b)
  ==>
    (is_book_sorted b.combined)
;;

:disable books_sorted
:disable attempt_recovery

(* PROVED *)

theorem[rw] attempt_recovery_maintains_sorted_books_1 (s) =
  (books_sorted s.books
   && snapshots_sorted_opt s.channels.last_snapshot)
  ==>
    (is_book_sorted (attempt_recovery s).books.multi)
;;

(* PROVED *)

theorem[rw] attempt_recovery_maintains_sorted_books_2 (s) =
  (books_sorted s.books
   && snapshots_sorted_opt s.channels.last_snapshot)
  ==>
    (is_book_sorted (attempt_recovery s).books.implied)
;;

(* PROVED *)

theorem[rw] attempt_recovery_maintains_sorted_books_3 (s) =
  (books_sorted s.books
   && snapshots_sorted_opt s.channels.last_snapshot)
  ==>
    (is_book_sorted (attempt_recovery s).books.combined)
;;

:disable attempt_recovery

(* Book sorting invariants *)

:disable snapshots_sorted_opt
:disable attempt_recovery

(* PROVED *)

theorem[rw] book_always_sorted_recovery_1 (s, m, c : feed_state * message * channel_type) =
  (s.feed_status = InRecovery
   && books_sorted s.books
   && snapshots_sorted_opt s.channels.last_snapshot)
  ==>
    (books_sorted (process_msg_recovery (s,m,c) ).books)
;;

(* PROVED *)

theorem[rw] book_always_sorted_recovery_2 (s, m, c : feed_state * message * channel_type) =
  (books_sorted (process_msg_recovery (s,m,c)).books
   && snapshots_sorted_opt (process_msg_recovery (s,m,c) ).channels.last_snapshot)
  ==>
    (books_sorted (attempt_recovery (process_msg_recovery (s,m,c))).books)
;;

(* PROVED *)

theorem[rw] book_always_sorted_recovery_3 (s, m, c : feed_state * message * channel_type) =
  (s.feed_status = InRecovery
   && books_sorted s.books
   && snapshots_sorted_opt s.channels.last_snapshot)
  ==>
    (books_sorted (process_msg_recovery (s,m,c) ).books)
;;

(* PROVED *)

theorem[rw] book_always_sorted_recovery_4 (s, m, c : feed_state * message * channel_type) =
  (s.feed_status = InRecovery
   && books_sorted s.books
   && snapshots_sorted_opt s.channels.last_snapshot)
  ==>
    (snapshots_sorted_opt (process_msg_recovery (s,m,c) ).channels.last_snapshot)
;;

(* PROVED *)

theorem[rw] book_always_sorted_recovery (s) =
  (s.feed_status = InRecovery
   && books_sorted s.books
   && snapshots_sorted_opt s.channels.last_snapshot)
  ==>
    (books_sorted (one_step s).books)
;;

(* End of InRecovery book sorting lemmas *)
(* ************************************************************** *)



(* Beginning of Normal book sorting lemmas *)

:enable reset_books books_sorted

theorem[rw] reset_books_sorted (b) =
  books_sorted (reset_books b)
;;

:disable reset_books books_sorted
:enable books_sorted

theorem[rw] books_sorted_ignores_depth_and_status_1 (b) =
  (books_sorted b)
  ==>
    (books_sorted { b with b_status = Publishable })
;;

theorem[rw] books_sorted_ignores_depth_and_status_2 (b) =
  (books_sorted b)
  ==>
    (books_sorted { b with b_status = Empty })
;;

theorem[rw] books_sorted_ignores_depth_and_status_3 (b, n) =
  (books_sorted b)
  ==>
    (books_sorted { b with b_status = Empty; book_depth = n })
;;

theorem[rw] books_sorted_ignores_depth_and_status_4 (b, n) =
  (books_sorted b)
  ==>
    (books_sorted { b with b_status = Publishable; book_depth = n })
;;

theorem[rw] process_msg_normal_sorted ( s , m : feed_state * message ) =
  (s.feed_status = Normal
   && books_sorted s.books
   && snapshots_sorted_opt s.channels.last_snapshot)
  ==>
    (books_sorted (process_msg_normal (s,m) ).books)
;;

:disable process_msg_normal
:enable is_side_sorted_raw order_higher_ranked

theorem[rw] add_levels_maintains_sorted (side, os) =
  (is_side_sorted_raw (side, os))
  ==>
    (is_side_sorted_raw (side, (add_levels os)))
;;

:disable is_side_sorted_raw order_higher_ranked
:disable add_levels
:enable is_book_sorted

theorem[rw] recalc_combined_maintains_sorted (bs) =
  (books_sorted bs)
  ==>
    (books_sorted (recalc_combined bs))
;;

theorem[rw] is_book_sorted_reduction (b) =
  (is_side_sorted_raw (OrdBuy, b.buys)
   && is_side_sorted_raw (OrdSell, b.sells))
  ==>
    (is_book_sorted b)
;;

:disable is_book_sorted

theorem[rw] book_always_sorted_recovery_final (s) =
  (s.feed_status = Normal
   && books_sorted s.books
   && snapshots_sorted_opt s.channels.last_snapshot)
  ==>
    (books_sorted (one_step s).books)
;;

theorem[rw] book_always_sorted (s) =
  (books_sorted s.books
   && snapshots_sorted_opt s.channels.last_snapshot)
    ==>
  (books_sorted (one_step s).books)
;;
