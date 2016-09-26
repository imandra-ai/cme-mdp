let book_status_to_json : (book_status -> Yojson.Basic.json) = function
    | Empty       -> `String "Empty"
    | Publishable -> `String "Publishable"
;;

let books_to_json (b : books) : Yojson.Basic.json = `Assoc [
    ( "BooksDepth" , `Int b.book_depth );
    ( "Multi"      , b.multi    |> book_to_json );
    ( "Implied"    , b.implied  |> book_to_json );
    ( "Combined"   , b.combined |> book_to_json );
    ( "Status"     , b.b_status |> book_status_to_json );
];;

let internal_msg_to_json (im : internal_msg) : Yojson.Basic.json = `Assoc [
    ( "MessageTime" , `Int im.im_time );
    ( "Books" , im.im_books |> books_to_json   );
    ( "Cache" , `List ( List.map inc_refresh_to_json im.im_cache ) )
];;

let itransitions_to_json (ims : internal_msg list) : Yojson.Basic.json = `Assoc [
    ("InternalTransitions" , `List ( List.map internal_msg_to_json ims ) )
];;
