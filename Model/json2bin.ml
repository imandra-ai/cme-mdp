let bin_of_packet = 
    let open CME in function
    | SnapshotPacket   p -> CME_writer.snap_packet_write ( Binparser.create_out () ) [p]
    | IncRefreshPacket p -> CME_writer.ref_packet_write  ( Binparser.create_out () ) [p]

let get_packet_time = 
    let open CME in function
    | SnapshotPacket   p -> p.sp_header.ph_sending_time
    | IncRefreshPacket p -> p.rp_header.ph_sending_time

let process infile outfile =
    Yojson.Basic.from_file infile
        |> CME_json.packets_of_json
        |> List.map bin_of_packet 
        |> Binparser.concat_out
        |> Binparser.write_out outfile


let process_pcap infile outfile pcapfile =
    let module J = Yojson.Basic in
    let pcap_config key = J.from_file pcapfile |> J.Util.member key |> J.Util.to_string in
    let info = {
        Binparser.src_mac  =  pcap_config "srcMAC"  ;
        Binparser.dst_mac  =  pcap_config "dstMAC"  ;
        Binparser.src_ip   =  pcap_config "srcIP"   ;
        Binparser.dst_ip   =  pcap_config "dstIP"   ;
        Binparser.src_port =  pcap_config "srcPort" ;
        Binparser.dst_port =  pcap_config "dstPort" 
    } in
    let write_packet pck = 
        let payload = bin_of_packet pck in
        let time = get_packet_time pck in 
        Binparser.concat_out [ Binparser.make_pcap_header info time payload ; payload ]
    in        
    let fileheader = Binparser.write_pcap_file_info () in
    Yojson.Basic.from_file infile
        |> CME_json.packets_of_json
        |> List.map write_packet 
        |> fun ps -> fileheader::ps
        |> Binparser.concat_out 
        |> Binparser.write_out outfile

let command =
    Core.Std.Command.basic
        ~summary:"Reads packets from a JSON file and writes down their binary representaion."
        ~readme:(fun () -> "")
        Core.Std.Command.Spec.(
            empty
            +> flag "-i" (required string) ~doc:"Input JSON file "
            +> flag "-o" (required string) ~doc:"Output binary file (all packets are written down)"
            +> flag "-p" (optional string) ~doc:"Request PCAP format for output"
        )
        ( fun infile outfile pcap () -> match pcap with
            | None   -> process infile outfile  
            | Some n -> process_pcap infile outfile n 
        )

let () =
    Core.Std.Command.run ~version:"1.0" ~build_info:"RWO" command

