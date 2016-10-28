#use "topfind";;
#require "str";;
#require "unix";;
#require "yojson";;

:load Model/CME_Types.ml
:load Model/CME.ml
:load Model/CME_Exchange.ml
:load_ocaml Printers/CME_json.ml
:load_ocaml Printers/CME_Exchange_json.ml
:load_ocaml Printers/CME_Internal_json.ml

:shadow off

let process_json filename = 
    let estate = Yojson.Basic.from_file filename |> exchange_state_of_json  in
    let packets = estate.pac_queue in
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

            cycle_hist_a = clean_cycle_hist (1,2);      
            cycle_hist_b = clean_cycle_hist (1,2);

            last_seq_processed = 0;
            cache = [];       

            last_snapshot = None
        };

        feed_status = Normal;
        internal_changes = [];
        cur_time = 0;
    } in
    let state = simulate state in
    state
;;


let scandir dirname  = 
    let dirhandle = Unix.opendir dirname in
    begin
    try
    while true do
        match Unix.readdir dirhandle with
        | "." | ".." -> ()
        | file -> begin 
            let file = dirname ^ "/" ^ file in
            let re = Str.regexp ".*/state_\(.*\)\.json$" in
            Printf.printf "Json file %s ... " file;
            if Str.string_match re file 0 then
                let code  = Str.matched_group 1 file in
                let state = process_json file |> simulate in
                let ch = state.channels in 
                ch.processed_ref_a     |>      packets_to_json |> Yojson.Basic.to_file ("newGenerated/channel_ref_a_"  ^ code ^ ".json");
                ch.processed_ref_b     |>      packets_to_json |> Yojson.Basic.to_file ("newGenerated/channel_ref_b_"  ^ code ^ ".json");
                ch.processed_snap_a    |>      packets_to_json |> Yojson.Basic.to_file ("newGenerated/channel_snap_a_" ^ code ^ ".json");
                ch.processed_snap_b    |>      packets_to_json |> Yojson.Basic.to_file ("newGenerated/channel_snap_b_" ^ code ^ ".json");
                state.internal_changes |> itransitions_to_json |> Yojson.Basic.to_file ("newGenerated/internal_"       ^ code ^ ".json");
                Printf.printf "done\n"
            else Printf.printf "skipped\n";
        end
    done
    with
    | End_of_file -> ()
    end;
    Unix.closedir dirhandle
;;

:shadow on
