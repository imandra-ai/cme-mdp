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

