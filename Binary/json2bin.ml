let process infile outfile =
    let write_packet packet = CME_writer.packet_write ( Binparser.create_out () ) packet in
    Yojson.Basic.from_file infile
        |> CME_json.packets_of_json
        |> List.map ( write_packet 1469570654 )
        |> Binparser.concat_out
        |> Binparser.write_out outfile

let command =
    Core.Std.Command.basic
        ~summary:"Reads packets from a JSON file and wirtes down the binary file."
        ~readme:(fun () -> "")
        Core.Std.Command.Spec.(
            empty
            +> flag "-i" (required string) ~doc:"Input JSON file "
            +> flag "-o" (required string) ~doc:"Output binary file (all packets are written down)"
        )
        ( fun infile outfile () -> process infile outfile )

let () =
    Core.Std.Command.run ~version:"1.0" ~build_info:"RWO" command

