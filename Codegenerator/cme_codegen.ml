open Utils

let preamble = {
    typedecl = "";
    writer = "open Message_types";
    reader = String.concat "\n" [ 
        "open Message_types                                          ";
        "let rec foldi (i,b) f bits =                                ";
        "    if i <= 0 then                                          ";
        "        [], bits                                            ";
        "    else                                                    ";
        "        let block, bits = Binparser.cut_block bits b in     ";
        "        let v,  _  = f block in                             ";
        "        let tl, bits = foldi (i-1, b) f bits in             ";
        "        v::tl, bits                                         "
    ]
}

let preparedir name =
    if Sys.file_exists name then
        if Sys.is_directory name then ()
        else raise (Failure ("File \"" ^ name ^ "\" is not a directory") ) 
    else
        Unix.mkdir name 0o777

let write_code outdir code =
    let typesml  = open_out (outdir ^ "/message_types.ml") in
    let readerml = open_out (outdir ^ "/readers.ml") in
    let writerml = open_out (outdir ^ "/writers.ml") in
    output_string typesml  code.typedecl;
    output_string readerml code.reader;
    output_string writerml code.writer;
    close_out typesml;
    close_out readerml;
    close_out writerml
    

let process filename outdir =
    let context_hash = Hashtbl.create 30 in
    let types  = Cmetypes.get_types filename in
    let msgs = Cmemessages.get_types filename in
    let tentry x =  "    | M_" ^ x.Cmemessages.name ^ " of " ^ Cmemessages.name x in
    let rentry x =  "    | " ^ string_of_int x.Cmemessages.id ^ 
                    " -> M_" ^ x.Cmemessages.name ^ " ( read_msg_" ^ x.Cmemessages.name ^ " (bits, gbits))" in
    let wentry x =  "    | " ^ "M_" ^ x.Cmemessages.name ^ " x -> " ^
                    " ( write_msg_" ^ x.Cmemessages.name ^ " x )" in
    let code = List.fold_left codecat preamble  ( 
        List.map (Cmetypes.to_ocaml context_hash) types @ 
        List.map Cmemessages.to_ocaml msgs              @
        [{
            typedecl = String.concat "\n" [
                "type message = ";
                msgs |> List.map tentry  |> String.concat "\n" 
            ];
            reader = String.concat "\n" [
                "let read_message bits md =";
                "    let gbits = Binparser.skip bits md.Binparser.block_length in";
                "    match md.Binparser.template_id with";
                msgs |> List.map rentry  |> String.concat "\n" 
            ];
            writer = String.concat "\n" [
                "let write_message bits md msg =";
                "    let fields, groups = match msg with";
                msgs |> List.map wentry  |> String.concat "\n";
                "    in Binparser.write_message bits ( md, fields, groups ) "
            ]
        }]
    )
    in 
    preparedir outdir;
    write_code outdir code;
    print_string ( msgs |> List.map (fun x -> try Model_io.generate_ref_of x with _ -> "" ) |> String.concat "\n" )
;;

let command =
    Core.Std.Command.basic
        ~summary:"Processes template XML file, generating Ocaml code. "
        ~readme:(fun () -> "")
        Core.Std.Command.Spec.(
            empty
            +> flag "-i" (required string) ~doc:"Input XML file"
            +> flag "-d" (required string) ~doc:"Output directory name (files are overwritten if exist)"
        )
        (fun filename outdir () -> process filename outdir )

let () =
    Core.Std.Command.run ~version:"1.0" ~build_info:"RWO" command

