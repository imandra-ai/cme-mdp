#use "topfind";;
#require "unix";;
#require "yojson";;

:load Model/CME_Types.ml
:load Model/CME.ml
:load_ocaml Printers/CME_json.ml

:shadow off

let process_json filename = 
    let packets = Yojson.Basic.from_file filename |> packets_of_json in
    let state = {
        feed_sec_type = SecA;
        feed_sec_id   = 1; (* TODO Fix the secid shizm*)

        (* The books that we're maintaining, etc... *)
        books = {
            book_depth = 5;
            multi      = empty_book 5;
            implied    = empty_book 5;
            combined   = empty_book 5;
            b_status   = Empty
        };

        channels = {    
            unprocessed_packets = List.rev packets;

            processed_messages = [];
            processed_ref_a = [];
            processed_ref_b = [];
            processed_snap_a = [];
            processed_snap_b = [];

            cycle_hist_a = clean_cycle_hist;      
            cycle_hist_b = clean_cycle_hist;

            last_seq_processed = 0;
            cache = [];       

            last_snapshot = None
        };

        feed_status = Normal;
        internal_changes = [];
        cur_time = 0;
    } in
    (*let state = simulate state in*)
    state
;;


let scandir dirname  = 
    let dirhandle = Unix.opendir dirname in
    begin
    try
    while true do
        match Unix.readdir dirhandle with
        | "." | ".." -> ()
        | file -> Printf.printf "Json file %s\n" file
    done
    with
    | End_of_file -> ()
    end;
    Unix.closedir dirhandle
;;

let s = process_json "generated/test_b1611ca24dd3749cc4852784eb2d3692_1.json";;

let rec nsim (s, n) = match n with
    | 0 -> s | n -> nsim (one_step s, n - 1)
;;

let getmsg nn = 
    match List.hd (List.hd (nsim (s,nn)).channels.unprocessed_packets).packet_messages 
    with RefreshMessage x -> x | _ -> failwith "" 
;;

(*    let msg = getmsg 1 in *)
let bks msg = 
    let m = s.books.implied in
    let sells = bk_new (m.sells, OrdSell, msg.rm_price_level, msg.rm_entry_size, msg.rm_entry_px, msg.rm_num_orders) in
    let books = { s.books with implied = { m with sells = sells; }} in
    books
;;
          (*  clean_multi_depth_book (books') *)
    
:shadow on
