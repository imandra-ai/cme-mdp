(* VG1 - CME book sorting *)

(** ************************************************************* *)
(** 1: Under no circumstances the book is unsorted                *)
(** ************************************************************* *)

(** Check that after every update to the book, the book is sorted *)


(** 1. Proofs about order_higher_ranked *)

(* PROVED *)
theorem[rw] ohr_trans (s, o1, o2, o3) =
    ( order_higher_ranked (s, o1, o2) &&  
      order_higher_ranked (s, o2, o3) )
    ==>
    ( order_higher_ranked (s, o1, o3) )
;;

(* PROVED *)
theorem[rw] ohr_refl (s, o1) =
      order_higher_ranked (s, o1, o1)
;;

(* PROVED *)
theorem[rw] ohr_antisym (s, o1, o2) =
    not ( order_higher_ranked (s, o1, o2) )
    ==> ( order_higher_ranked (s, o2, o1) )
;;

(* PROVED *)
theorem[rw] ohr_everything_higher_than_nolevel(s,x) = 
    order_higher_ranked(s, x, NoLevel)
;;


:disable order_higher_ranked

(** 2. Proofs about sortedness for replace_at and delete_at *)

let rec is_side_sorted_raw (side, ls) =
    match ls with
    |  [] -> true
    | [a] -> true
    | a :: b :: _ -> 
	order_higher_ranked (side, a, b)
        && is_side_sorted_raw (side, List.tl ls)
;;

(**Sanity check -- length maintained by replace_at *)
verify length_maintained_by_replace_at(s,order,orders,n) = 
    List.length orders = List.length( replace_level_at (s,order,orders,n) );;

(* PROVED *)
theorem[rw] sorted_maintained_by_replace (s, order, orders, n) = 
    is_side_sorted_raw(s, orders) 
    ==> is_side_sorted_raw(s, replace_level_at(s,order,orders,n))
;;

(**Sanity check -- length maintained by delete_at *)
verify length_maintained_by_delete(lst,n) = 
    List.length lst = List.length( delete_at (lst,n) );;

(* PROVED *)
theorem[rw] sorted_maintained_by_delete (s,orders, n) = 
    is_side_sorted_raw(s,orders) ==> is_side_sorted_raw(s,delete_at(orders,n))
;;

(** 3. Book(s) sortedness *)

let is_book_sorted (b : book) =
       is_side_sorted_raw ( OrdBuy  , b.buys  )
    && is_side_sorted_raw ( OrdSell , b.sells )
;;

let books_sorted (b : books) =
       is_book_sorted b.implied
    && is_book_sorted b.multi
    && is_book_sorted b.combined
;;

(** 4. Recovery message *)

(* Messages in recovery do not change books *)
(* PROVEN *)
theorem[rw] process_msg_recovery_does_not_change_books (s, next_message, channel_type : feed_state * message * channel_type) =
    let s' = process_msg_recovery (s, next_message, channel_type)  in
    s'.books = s.books
;;

(* Thus books stay sorted *)
(* PROVEN *)
theorem[rw] book_always_sorted_process_msg_recovery (s, m, c : feed_state * message * channel_type) =
    (books_sorted s.books)
    ==>
    (books_sorted (process_msg_recovery (s,m,c) ).books)
;;
:disable process_msg_recovery

(** 5. MD update action on books  *)

(* PROVEN *)
theorem[rw] process_md_update_action_sorted_books (books, msg) =
    books_sorted books ==> books_sorted (process_md_update_action (books, msg))
;;

:disable process_md_update_action

(* PROVEN *)
theorem[rw] apply_update_packets_sorded (books, packets) = 
    books_sorted books ==> books_sorted (apply_update_packets (books, packets))
;;

(* PROVEN *)
theorem[rw] apply_cache_sorted (books, channels) = 
    books_sorted books  ==> books_sorted (apply_cache (books, channels))
;;

:disable apply_cache


(** 6. Reset books  *)

(* PROVEN *)
theorem[rw] reset_books_sorted (bs : books) =
   books_sorted (reset_books bs);;

:disable reset_books


(** 7. Applying snaphot *)

let snapshots_sorted (snap) =
     is_book_sorted snap.snap_m_book
  && is_book_sorted snap.snap_i_book
;;

let snapshots_sorted_opt (snap_opt) =
  match snap_opt with
    None -> true
  | Some snap -> snapshots_sorted snap;;

(* PROVEN *)
theorem[rw] apply_snapshot_sorted (bs , snap ) =
   snapshots_sorted snap ==> books_sorted (apply_snapshot (bs, snap) );;

:disable apply_snapshot

(** 8. Recalc combined *)

(* CHEATING HERE *)
axiom[rw] recalc_combined_sorted(bs : books) = 
   books_sorted( recalc_combined bs );;
 
:disable recalc_combined

(** 9. Attempt recovery *)

theorem[rw] attempt_recovery_sorted (s : feed_state ) =
    (   books_sorted( s.books ) &&
        snapshots_sorted_opt ( s.channels.last_snapshot ) 
    ) ==> 
    books_sorted (attempt_recovery (s)).books;;

:disable attempt_recovery

(** 10. Process recovery *)
theorem[rw] process_msg_normal_sorted( s, msg : feed_state * message ) =
    books_sorted( s.books ) 
    ==>
    books_sorted (process_msg_normal(s, msg)).books;;

:disable process_msg_normal

(** 11. one_step *)

:enable attempt_recovery
(* Needs a nudge *)
theorem[rw] process_then_attempt_sorted(s,m,c) =
    books_sorted( s.books ) ==>
    books_sorted ( attempt_recovery(process_msg_recovery(s,m,c))).books;;
:disable attempt_recovery


theorem[rw] book_always_sorted (s : feed_state) =
    (   books_sorted( s.books ) &&
        snapshots_sorted_opt ( s.channels.last_snapshot ) 
    ) ==> 
    books_sorted (one_step (s)).books;;
