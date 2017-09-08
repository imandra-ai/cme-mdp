let process_pcap infile outfile =
    let module J = Yojson.Basic in
    let bits = Binparser.open_in infile in
    let bits = Binparser.read_pcap_file_info ~vrb:true bits in
    let rec read current bits = 
        try 
            let (md, payld, bits) = Binparser.get_pcap_message bits in 
            Printf.printf " Read template %d \n" md.template_id;
            flush_all ();
            let packets = CME_reader.read_packets payld md in
            read (packets :: current) bits
        with _ -> 
            Printf.printf "Caught \n";
            current 
        in
    read [] bits |> List.concat
              |> CME_json.packets_to_json 
              |> Yojson.Basic.to_file outfile
              
              
let process infile outfile =
    let module J = Yojson.Basic in
    let bits = Binparser.open_in infile in
    let rec read current bits = 
        try 
            let (md, payld, bits) = Binparser.get_message bits in 
            Printf.printf " Read template %d \n" md.template_id;
            flush_all ();
            let packets = CME_reader.read_packets payld md in
            read (packets :: current) bits
        with _ -> 
            Printf.printf "Caught \n";
            current 
        in
    read [] bits |> List.concat
                 |> CME_json.packets_to_json 
                 |> Yojson.Basic.to_file outfile
 

let command =
    Core.Std.Command.basic
        ~summary:"Reads packets from a binary file and writes them down in JSONrepresentaion."
        ~readme:(fun () -> "")
        Core.Std.Command.Spec.(
            empty
            +> flag "-i" (required string) ~doc:"Input binary file "
            +> flag "-o" (required string) ~doc:"Output JSON file "
            +> flag "-p" (no_arg)          ~doc:"The input binary is in PCAP format"
        )
        ( fun infile outfile pcap () ->
              if pcap then process_pcap infile outfile 
                      else process      infile outfile  )

let () =
    Core.Std.Command.run ~version:"1.0" ~build_info:"RWO" command

