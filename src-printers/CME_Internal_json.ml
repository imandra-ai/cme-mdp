(* @meta[imandra_ignore] on @end *)
open CME_Types;;
open CME;;
open CME_json;;
(* @meta[imandra_ignore] off @end *)


let change_type_to_json : (book_change_type -> Yojson.Basic.json) = function
    | Book_Changed_to_InRecovery -> `String "Change To Recovery"
    | Book_Changed_to_Normal     -> `String "Change To Normal"
    | Book_Proc_Normal_Update    -> `String "Normal"
    | Book_Proc_Cache_Add        -> `String "Cache add"
    | Book_Proc_NotRelevant      -> `String "Not Relevant"
    | Book_Proc_Snap             -> `String "Snapshot"
;;



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
    ( "Cache" , `List ( List.map inc_refresh_to_json im.im_cache ) );
    ( "ChangeType", im.im_change_type |> change_type_to_json )
];;

let itransitions_to_json (ims : internal_msg list) : Yojson.Basic.json = `Assoc [
    ("InternalTransitions" , `List ( List.map internal_msg_to_json ims ) )
];;
