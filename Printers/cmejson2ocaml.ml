let process infile  =
    print_string "let starting_state = ";
    Yojson.Basic.from_file infile 
        |> CME_Exchange_json.exchange_state_of_json 
        |> CME_Exchange_json.exchange_state_to_ocaml 
        |> print_string;
    print_string ";;"

let command =
    Core.Std.Command.basic
        ~summary:"Reads the CME Exchange state from a JSON file and wirtes the OCaml representaiton of it to stdout."
        ~readme:(fun () -> "")
        Core.Std.Command.Spec.(
            empty
            +> flag "-i" (required string) ~doc:"Input JSON file "
        )
        ( fun infile () -> process infile )

let () =
    Core.Std.Command.run ~version:"1.0" ~build_info:"RWO" command

