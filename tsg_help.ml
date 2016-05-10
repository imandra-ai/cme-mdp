(* Some TSG help lemmata.
   We'll automate the construction of these eventually... *)

:!p
(defthm book_depth_is_int
        (implies (|books-P| books)
                 (integerp (|books->book_depth| books)))
 :rule-classes (:rewrite :type-prescription))
;;

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
  (defthm state_channels_type
          (implies (|feed_state-P| s)
                   (|channels-P| (|feed_state->channels| s)))
  :rule-classes (:type-prescription :rewrite))
;;

:!p
  (defthm ref_a_type
          (implies (|channels-P| x)
                   (|ref_channel_state-P| (|channels->ref_a| x)))
  :rule-classes (:type-prescription :rewrite))
;;

:!p
  (defthm ref_b_type
          (implies (|channels-P| x)
                   (|ref_channel_state-P| (|channels->ref_b| x)))
  :rule-classes (:type-prescription :rewrite))
;;

:!p
  (defthm snap_a_type
          (implies (|channels-P| x)
                   (|snap_channel_state-P| (|channels->snap_a| x)))
  :rule-classes (:type-prescription :rewrite))
;;

:!p
  (defthm snap_b_type
          (implies (|channels-P| x)
                   (|snap_channel_state-P| (|channels->snap_b| x)))
  :rule-classes (:type-prescription :rewrite))
;;

:!p
  (defthm packet_list_car_type
          (implies (|ref_channel_state-P| x)
                   (|ref_packet_LIST-P| (|ref_channel_state->r_unproc_packets| x)))
   :rule-classes (:type-prescription :rewrite))
;;

:!p
  (defthm packet_list_car_type_snap
          (implies (|snap_channel_state-P| x)
                   (|snap_packet_LIST-P| (|snap_channel_state->s_unproc_packets| x)))
   :rule-classes (:type-prescription :rewrite))
;;

:!p
  (defthm packet_list_car_type_2_ref
          (implies (and (|ref_channel_state-P| x)
                          (|ref_channel_state->r_unproc_packets| x))
                   (|ref_packet-P| (CAR (|ref_channel_state->r_unproc_packets| x))))
   :rule-classes (:type-prescription :rewrite))
;;

:!p
  (defthm packet_list_car_type_2_snap
          (implies (and (|snap_channel_state-P| x)
                          (|snap_channel_state->s_unproc_packets| x))
                   (|snap_packet-P| (CAR (|snap_channel_state->s_unproc_packets| x))))
   :rule-classes (:type-prescription :rewrite))
;;

  (*
:!p
  (defthm packet_fc_ref
          (implies (and (|feed_state-P| X1)
                          (|ref_channel_state->r_unproc_packets|
                           (|channels->ref_a| (|feed_state->channels| X1))))
                   (|ref_packet-P|
                    (CAR (|ref_channel_state->r_unproc_packets|
                          (|channels->ref_a| (|feed_state->channels| X1))))))
   :rule-classes (:forward-chaining))
;;

:!p
  (defthm packet_fc_ref_b
          (implies (and (|feed_state-P| X1)
                          (|ref_channel_state->r_unproc_packets|
                           (|channels->ref_b| (|feed_state->channels| X1))))
                   (|ref_packet-P|
                    (CAR (|ref_channel_state->r_unproc_packets|
                          (|channels->ref_b| (|feed_state->channels| X1))))))
   :rule-classes (:forward-chaining))
;;

:!p
  (defthm packet_fc_snap
          (implies (and (|feed_state-P| X1)
                          (|snap_channel_state->s_unproc_packets|
                           (|channels->snap_a| (|feed_state->channels| X1))))
                   (|snap_packet-P|
                    (CAR (|snap_channel_state->s_unproc_packets|
                          (|channels->snap_a| (|feed_state->channels| X1))))))
   :rule-classes (:forward-chaining))
;;

:!p
  (defthm packet_fc_snap_b
          (implies (and (|feed_state-P| X1)
                          (|snap_channel_state->s_unproc_packets|
                           (|channels->snap_b| (|feed_state->channels| X1))))
                   (|snap_packet-P|
                    (CAR (|snap_channel_state->s_unproc_packets|
                          (|channels->snap_b| (|feed_state->channels| X1))))))
   :rule-classes (:forward-chaining))
;;
   *)
