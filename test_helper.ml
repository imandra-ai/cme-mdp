
open CME;;

(** The following two functions are used to tranform internal packet list types to those
    we would see in actual protocol. See the note above. *)
let rec merge_ref_packets ( old_p, new_p : ref_packet list * ref_packet_list list ) =
    match old_p with 
    | [] -> new_p
    | x::xs -> 
        let new_p' = (
            match new_p with 
                | [] -> [ { rpl_header = x.rp_header;
                            rpl_msgs = [ x.rp_msg ] }]
                | y::ys ->
                    if x.rp_header = y.rpl_header then
                        (** in this case we just append it to the list of current ones *)
                        { y with rpl_msgs = y.rpl_msgs @ [ x.rp_msg ] } :: ys
                    else
                        let new_pl = { rpl_header = x.rp_header; rpl_msgs = [ x.rp_msg ]} in 
                        new_pl :: new_p
        )
    in 
        merge_ref_packets (xs, new_p')
;;

(** merge_snap_packets: tranforms list of individual packets into 
    snap_packet list -> snap_packet_list *)
let rec merge_snap_packets ( old_p, new_p : snap_packet list * snap_packet_list list) = 
    match old_p with 
    | [] -> new_p 
    | x::xs -> 
        let new_p' = (
            match new_p with
                | [] -> [ { spl_header = x.sp_header;
                            spl_snap = [ x.sp_snap ]}; ]
                | y::ys ->
                    if x.sp_header = y.spl_header then 
                        { y with spl_snap = y.spl_snap @ [ x.sp_snap ]} :: ys
                    else 
                        let new_pl = { spl_header = x.sp_header; spl_snap = [ x.sp_snap ]} in 
                        new_pl :: new_p
        )
    in 
        merge_snap_packets (xs, new_p')
;;

(** Create a list of internal snapshot messages *)
let rec explode_snap_packets (old_p, new_p : snap_packet_list list * snap_packet list) = 
    match old_p with
    | [] -> new_p
    | x::xs ->
        match x.spl_snap with 
            | [] -> explode_snap_packets (xs, new_p)
            | y::ys ->
                let new_p' = { sp_header = x.spl_header; sp_snap = y } in 
                let x' = { x with spl_snap = ys } in 
                explode_snap_packets (x'::xs, new_p'::new_p)
;;

(** And do the same for incremental refresh packets *)
let rec explode_ref_packets (old_p, new_p : ref_packet_list list * ref_packet list) =
    match old_p with
    | [] -> new_p
    | x::xs -> 
        match x.rpl_msgs with 
            | [] -> explode_ref_packets (xs, new_p)
            | y::ys ->
                let new_p' = { rp_header = x.rpl_header; rp_msg = y; } in
                let x' = { x with rpl_msgs = ys } in 
                explode_ref_packets (x'::xs, new_p'::new_p
;;

(** This will return a copy of channels but only for the earliest 
    time_stamps *)
let get_next_msgs (ch : channels) = 
  { ch with 
        inc_a = [ List.hd ch.inc_a ];
        inc_b = [ List.hd ch.inc_b ];

        ref_a = [ List.hd ch.ref_a ];
        ref_b = [ List.hd ch.ref_b ];
  }
;;

(** This is the complement of the function above *)
let update_channels (ch : channels ) =
    { ch with
        inc_a = List.tl ch.inc_a; 
        inc_b = List.tl ch.inc_b;

        ref_a = List.tl ch.ref_a;
        ref_b = List.tl ch.ref_b;
    }
;;






