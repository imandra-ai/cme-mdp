(* @meta[imandra_ignore] on @end *)
open CME_Types;;
open CME;;
open CME_json;;
(* @meta[imandra_ignore] off @end *)

(* ****  Serializers for book_change  **** *)
let book_change_to_json, book_change_of_json =
    let m2j : (book_change_type * Yojson.Basic.json ) list = [
        ( Book_Changed_to_InRecovery , `String "Change_to_InRecovery" );
        ( Book_Changed_to_Normal     , `String "Change_to_Normal"     );
        ( Book_Proc_Normal_Update    , `String "Normal_Update"        );
        ( Book_Proc_Cache_Add        , `String "Cache_Add"            );
        ( Book_Proc_NotRelevant      , `String "NotRelevant"          );
        ( Book_Proc_Snap             , `String "Snapshot"             ) ] in
    let j2m = List.map (fun (a,b) -> b,a ) m2j in
    (fun x -> List.assoc x m2j), (fun x -> List.assoc x j2m  )
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

let internal_msg_to_json (im : internal_msg) : Yojson.Basic.json = 
    match im.im_change_type with
    | Book_Proc_NotRelevant -> `Assoc [("ChangeType", book_change_to_json im.im_change_type)]
    | change -> `Assoc [
        ( "ChangeType"  , book_change_to_json change);
        ( "MessageTime" , `Int im.im_time           );
        ( "Books" , im.im_books |> books_to_json    );
        ( "Cache" , `List ( List.map inc_refresh_to_json im.im_cache ) )
];;

let itransitions_to_json (ims : internal_msg list) : Yojson.Basic.json = `Assoc [
    ("InternalTransitions" , `List ( List.map internal_msg_to_json ims ) )
];;
