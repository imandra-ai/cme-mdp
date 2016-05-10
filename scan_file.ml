let dump_msg filename num =
    let data = Binparser.open_in filename in
    let rec read bits = 
        let (md, payld, bits) = Binparser.get_message bits in 
        if md.seqence_num = Int32.of_int num then (
            print_string ("Found message #" ^ string_of_int num ^ "\n");
            print_string ( Binparser.metadata_to_string md  ^ "\n");
            Binparser.hexdump payld )
        else if md.seqence_num > Int32.of_int num then
            ( print_string ("Overshoot. Latest message #" ^ Int32.to_string md.seqence_num ^ "\n") )
        else
            read bits in (* That should be tail-recursive *)
    read data;;


let process filename =
    let bits = Binparser.open_in filename in
    let rec read bits =
         let (id ,payld, bits) = Binparser.get_message ~vrb:true bits in 
         read bits in (* That should be tail-recursive *)
    try read bits with _ -> print_string "Scanning terminated\n";;

let command =
    Core.Std.Command.basic
        ~summary:"Test block reading -- scans file and prints header metadata."
        ~readme:(fun () -> "")
        Core.Std.Command.Spec.(
            empty
            +> flag "-i" (required string) ~doc:"Input binary data file."
            +> flag "-d" (optional int   ) ~doc:"Message number to hexdump. "
        )
        ( fun filename msg () -> 
            match msg with 
                | None -> process filename 
                | Some num -> dump_msg filename num
        )

let () =
    Core.Std.Command.run ~version:"1.0" ~build_info:"RWO" command

