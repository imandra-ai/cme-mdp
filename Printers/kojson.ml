module Kojson = struct
type json = Assoc   of ( string * json ) list
          | Int     of int
          | String  of string
          | List    of json list

let rec to_string = function
    | Int    i -> string_of_int i
    | String s -> s |> Printf.sprintf "\"%s\""
    | List   l -> l |> List.map to_string 
                    |> String.concat ","
                    |> Printf.sprintf "[%s]" 
    | Assoc  l -> l |> List.map (fun (k,v) -> Printf.sprintf "\"%s\":%s" k (to_string v))
                    |> String.concat ","
                    |> Printf.sprintf "{%s}" 
end;;
