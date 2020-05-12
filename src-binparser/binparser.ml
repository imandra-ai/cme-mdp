type primitive_type = 
    | Int8 
    | Int16 
    | Int32 
    | Int64
    | Uint8 
    | Uint16 
    | Uint32 
    | Uint64
    | Decimal 
    | Decimal32 
    | Decimal64
    | Unknown
    | Char
    | String
    | Maturity


let string_to_pt = function
    | "int8"   -> Int8  
    | "int16"  -> Int16 
    | "int32"  -> Int32 
    | "int64"  -> Int64 
    | "uint8"  -> Uint8 
    | "uint16" -> Uint16
    | "uint32" -> Uint32
    | "uint64" -> Uint64
    | "char"   -> Char  
    | _ -> Unknown

let pt_to_string = function
    | Int8   -> "int8"   
    | Int16  -> "int16"  
    | Int32  -> "int32"  
    | Int64  -> "int64"  
    | Uint8  -> "uint8"  
    | Uint16 -> "uint16" 
    | Uint32 -> "uint32" 
    | Uint64 -> "uint64" 
    | Char   -> "char"   
    | _      -> "Unknown"


let to_ocaml = function
    | Int8      -> "int"      
    | Int16     -> "int"     
    | Int32     -> "int32"     
    | Int64     -> "int64"     
    | Uint8     -> "int"     
    | Uint16    -> "int"    
    | Uint32    -> "int32"    
    | Uint64    -> "int64"    
    | Char      -> "char"      
    | String    -> "string"    
    |      _    ->  raise (Failure ("type not implemented") )

type in_t   = Bitstring.t
type out_t  = Bitstring.t list

let open_in       = Bitstring.bitstring_of_file
let create_out () = []
let write_out fn x = x |> List.rev |> Bitstring.concat 
                       |> (fun y -> Bitstring.bitstring_to_file y fn)

let out_to_in  x  = x |> List.rev |> Bitstring.concat 
let concat_out x  = 
    x   |> List.rev
        |> List.map ( fun y -> y |> List.rev |> Bitstring.concat )

let length        = Bitstring.bitstring_length 
let skip bits l   = Bitstring.dropbits (8*l) bits 
let hexdump       = Bitstring.hexdump_bitstring stdout 
let hexdump_o  x  = x |> List.rev |> Bitstring.concat |> Bitstring.hexdump_bitstring stdout 


(******************************************)
(**  Message Metadata                    **)
(******************************************)

type msg_metadata = {
    seqence_num : Int32.t;
        sent_ts : Int64.t;
     receive_ts : Int64.t;
           size : int;
       msg_size : int;
   block_length : int;
    template_id : int;
      schema_id : int
}

let metadata_to_string md = [
        "receiveTime = " ^ ( Int64.to_string md.receive_ts  ); 
        "size = "        ^ ( string_of_int   md.size        ); 
        "msgSeqNum = "   ^ ( Int32.to_string md.seqence_num );   
        "sendingTime = " ^ ( Int64.to_string md.sent_ts     ); 
        "msgSize = "     ^ ( string_of_int   md.msg_size    );       
        "block_length = "^ ( string_of_int   md.block_length);       
        "templateID = "  ^ ( string_of_int   md.template_id );    
        "schemaID = "    ^ ( string_of_int   md.schema_id   )
    ] |> String.concat ", "

(******************************************)
(** PCAP header reading                  **)
(******************************************)

let read_pcap_file_info ?(vrb=false) bits =
    match%bitstring bits with 
    {| _magic       : 32 : unsigned, int, littleendian;  
       major_verion : 16 : unsigned, int, littleendian;
       minor_verion : 16 : unsigned, int, littleendian;
       timezone     : 32 : unsigned, int, littleendian;
       ts_accuracy  : 32 : unsigned, int, littleendian;
       max_length   : 32 : unsigned, int, littleendian;
       link_layer   : 32 : unsigned, int, littleendian |} ->
    begin
        if vrb then ([
            "Read a PCAP header:" ;
            " - major version : " ^ string_of_int major_verion  ;
            " - minor version : " ^ string_of_int minor_verion  ;
            " - tz GMT offset : " ^ Int32.to_string timezone    ;
            " - ts accuracy   : " ^ Int32.to_string ts_accuracy ;
            " - max pck length: " ^ Int32.to_string max_length  ;
            " - lik layer code: " ^ Int32.to_string link_layer  ;
            ""]
            |> String.concat "\n" |> print_string )
        else ();
        Bitstring.dropbits (24 * 8) bits 
    end

let write_pcap_file_info () = 
    let%bitstring pcapinfo = {|
        0xa1b2c3d4l  : 32 : unsigned, int, littleendian;  (* Magic *)
        2  : 16 : unsigned, int, littleendian;  (* Major version *)
        4  : 16 : unsigned, int, littleendian;  (* Minor version *)
        0l : 32 : unsigned, int, littleendian;
        0l : 32 : unsigned, int, littleendian;
        0x0000ffffl  : 32 : unsigned, int, littleendian;
        0x00000001l  : 32 : unsigned, int, littleendian 
    |} in 
    [pcapinfo];;
   

let get_pcap_message ?(vrb=false) bits =
    match%bitstring bits with 
    {| receive_ts   : 32 : unsigned, int, littleendian;  
      _receive_ns   : 32 : unsigned, int, littleendian;  
       size         : 32 : unsigned, int, littleendian;
      _othersize    : 32 : unsigned, int, littleendian;
      _stuffA       : 64 : unsigned, int, littleendian;
      _stuffB       : 64 : unsigned, int, littleendian;
      _stuffC       : 64 : unsigned, int, littleendian;
      _stuffD       : 64 : unsigned, int, littleendian;
      _stuffE       : 64 : unsigned, int, littleendian;
      _stuffF       : 48 : unsigned, int, littleendian;
       seqence_num  : 32 : unsigned, int, littleendian;
       sent_ts      : 64 : unsigned, int, littleendian;
       msg_size     : 16 : unsigned, int, littleendian;
       block_length : 16 : unsigned, int, littleendian;
       template_id  : 16 : unsigned, int, littleendian;
       schema_id    : 16 : unsigned, int, littleendian |} ->
    let receive_ts = Int64.of_int32 receive_ts in
    let size = Int32.to_int size in
    let md = { receive_ts; size; seqence_num; sent_ts;     
               msg_size; template_id; schema_id; block_length } in (
    if vrb then (
        print_string ( (bits |> Bitstring.bitstring_length |> string_of_int) ^ " bytes to go, ") ;
        print_string ( metadata_to_string md ^ "\n") )
    else () );
    ( md , 
      Bitstring.subbitstring bits (84*8)  ( (16 + size - 84) * 8),
      Bitstring.dropbits ((16 + size) * 8) bits 
    )

type pcap_info = {
    src_mac : string;
    dst_mac : string;
    src_ip  : string;
    dst_ip  : string;
    src_port: string;
    dst_port: string
}

let pcap_header ts size = 
    let size, ts = Int32.of_int size, Int32.of_int ts in 
    [%bitstring {|
        ts   : 32 : unsigned, int, littleendian;
        0l   : 32 : unsigned, int, littleendian;
        size : 32 : unsigned, int, littleendian;
        size : 32 : unsigned, int, littleendian |} 
    ]

let eth_header smac dmac = 
    let pack a b c d e f = [%bitstring {|
        a : 8 : unsigned, int, bigendian;
        b : 8 : unsigned, int, bigendian;
        c : 8 : unsigned, int, bigendian;
        d : 8 : unsigned, int, bigendian;
        e : 8 : unsigned, int, bigendian;
        f : 8 : unsigned, int, bigendian
    |} ] in 
    let%bitstring x801     = {| 0x8100 : 16 : unsigned, int, bigendian |} in
    let%bitstring bitfield = {| 
        0x00b3 : 16 : unsigned, int, bigendian ;
        0x0800 : 16 : unsigned, int, bigendian 
    |} in
    Bitstring.concat [
        Scanf.sscanf dmac "%x:%x:%x:%x:%x:%x" pack;
        Scanf.sscanf smac "%x:%x:%x:%x:%x:%x" pack;
        x801; bitfield
    ]
    
let ip_header sip dip tot_length = 
    let pack a b c d = [%bitstring {|
        a : 8 : unsigned, int, bigendian;
        b : 8 : unsigned, int, bigendian;
        c : 8 : unsigned, int, bigendian;
        d : 8 : unsigned, int, bigendian
    |} ] in
    let bytes = [%bitstring {|
        0x45 : 8 : unsigned, int, bigendian;
        0x00 : 8 : unsigned, int, bigendian;
        tot_length : 16 : unsigned, int, bigendian;
        0x9736 : 16 : unsigned, int, bigendian;
        0x4000 : 16 : unsigned, int, bigendian;
        0x39   :  8 : unsigned, int, bigendian;
        0x11   :  8 : unsigned, int, bigendian;
        0x1234 : 16 : unsigned, int, bigendian (* Checksum -- validation disabled *)
    |} ] in
    Bitstring.concat [
        bytes;
        Scanf.sscanf dip "%d.%d.%d.%d" pack;
        Scanf.sscanf sip "%d.%d.%d.%d" pack
    ]
    
let udp_header sport dport length = 
    let sport = int_of_string sport in
    let dport = int_of_string dport in
    [%bitstring {|
        dport  : 16 : unsigned, int, bigendian;
        sport  : 16 : unsigned, int, bigendian;
        length : 16 : unsigned, int, bigendian;
        0x1234 : 16 : unsigned, int, bigendian (* Checksum -- validation disabled *)
    |} ]

let time = ref 1461262423 
    
let make_pcap_header info _t payload =
    time := !time + 1;
    let size = payload |> List.map Bitstring.bitstring_length 
                       |> List.fold_left ( + ) 0
                       |> fun x -> x / 8
    in 
    [ Bitstring.concat [
        pcap_header !time  (size + 46) ;
        eth_header  info.src_mac   info.dst_mac;
        ip_header   info.src_ip    info.dst_ip   (size + 28);
        udp_header  info.src_port  info.dst_port (size +  8)
    ] ]

(******************************************)
(** Message header encoding and metadata **)
(******************************************)

let get_message ?(vrb=false) bits =
    match%bitstring bits with 
    {| receive_ts   : 64 : unsigned, int, littleendian;  
       size         : 16 : unsigned, int, littleendian;
       seqence_num  : 32 : unsigned, int, littleendian;
       sent_ts      : 64 : unsigned, int, littleendian;
       msg_size     : 16 : unsigned, int, littleendian;
       block_length : 16 : unsigned, int, littleendian;
       template_id  : 16 : unsigned, int, littleendian;
       schema_id    : 16 : unsigned, int, littleendian |} ->
    let md = { receive_ts; size; seqence_num; sent_ts;     
               msg_size; template_id; schema_id; block_length } in (
    if vrb then (
        print_string ( (bits |> Bitstring.bitstring_length |> string_of_int) ^ " bytes to go, ") ;
        print_string ( metadata_to_string md ^ "\n") )
    else () );
    ( md , 
      Bitstring.subbitstring bits (32*8)  ((10 + size - 32) * 8),
      Bitstring.dropbits ((10 + size) * 8) bits 
    )

let write_message _ (md, bits, gbits) =
    let bits = bits |> List.rev |> Bitstring.concat in
    let md = (* Calculating block_length *)
        if md.block_length = 0 
        then { md with block_length = 2 + Bitstring.bitstring_length bits / 8 }
        else md in
    let bits = (* Adding padding/truncating if necessary *)
        let l = Bitstring.bitstring_length bits in
             if l > 8*md.block_length then [ Bitstring.subbitstring bits 0 (8*md.block_length) ]
        else if l < 8*md.block_length then [ Bitstring.zeroes_bitstring (8*md.block_length - l); bits ]
        else [bits] in 
    let bits = (gbits @ bits) |> List.rev |> Bitstring.concat in
    let md = (* Calculating message size *)
        if md.size = 0 
        then { md with size = 32 + ( Bitstring.bitstring_length bits / 8 ) - 10 }
        else md in
    let md = (* Calculating msg_size *)
        if md.msg_size = 0 
        then { md with msg_size = md.size - 12 }
        else md in
    let%bitstring header =  
    {| md.receive_ts   : 64 : unsigned, int, littleendian;  
       md.size         : 16 : unsigned, int, littleendian;
       md.seqence_num  : 32 : unsigned, int, littleendian;
       md.sent_ts      : 64 : unsigned, int, littleendian;
       md.msg_size     : 16 : unsigned, int, littleendian;
       md.block_length : 16 : unsigned, int, littleendian;
       md.template_id  : 16 : unsigned, int, littleendian;
       md.schema_id    : 16 : unsigned, int, littleendian;
       0               : 16 : unsigned, int, littleendian |} in
    bits::header::[]
 

(******************************************)
(**     Group block header encoding      **)
(******************************************)

let get_group_info bits = 
    match%bitstring bits with
    {| b : 8 : unsigned, int, littleendian;
       _ : 8 : unsigned, int, littleendian;
       n : 8 : unsigned, int, littleendian |} -> (n, b, Bitstring.dropbits 24 bits)

let write_group_info  ostream ( n, b) = 
    let%bitstring hd = {| 
      b : 8 : unsigned, int, littleendian;
      0 : 8 : unsigned, int, littleendian;
      n : 8 : unsigned, int, littleendian |} in
    hd::ostream 

let cut_block bits bsize =
    let rest = min (Bitstring.bitstring_length bits)  (bsize*8) in
    (Bitstring.takebits rest bits, Bitstring.dropbits rest bits)

let append_padded bits nbits nbytes =
    let b = nbits |> List.rev |> Bitstring.concat in
    let l = Bitstring.bitstring_length b in
         if l > 8*nbytes then ( Bitstring.subbitstring b 0 (8*nbytes)     )::bits
    else if l < 8*nbytes then ( Bitstring.zeroes_bitstring (8*nbytes - l) )::b::bits
    else b::bits 
   
(******************************************)
(**     Integer type reading / witing    **)
(******************************************)


let read_int8   bits = match%bitstring bits with {| v :  8 :           int, littleendian |} -> v, Bitstring.dropbits  8 bits 
let read_int16  bits = match%bitstring bits with {| v : 16 :           int, littleendian |} -> v, Bitstring.dropbits 16 bits 
let read_int32  bits = match%bitstring bits with {| v : 32 :           int, littleendian |} -> v, Bitstring.dropbits 32 bits 
let read_int64  bits = match%bitstring bits with {| v : 64 :           int, littleendian |} -> v, Bitstring.dropbits 64 bits 
let read_uint8  bits = match%bitstring bits with {| v :  8 : unsigned, int, littleendian |} -> v, Bitstring.dropbits  8 bits 
let read_uint16 bits = match%bitstring bits with {| v : 16 : unsigned, int, littleendian |} -> v, Bitstring.dropbits 16 bits 
let read_uint32 bits = match%bitstring bits with {| v : 32 : unsigned, int, littleendian |} -> v, Bitstring.dropbits 32 bits 
let read_uint64 bits = match%bitstring bits with {| v : 64 : unsigned, int, littleendian |} -> v, Bitstring.dropbits 64 bits 

let write_int8   ostream  ( v:int     ) = let%bitstring bs = {| v: 8 :           int, littleendian |} in bs::ostream  
let write_int16  ostream  ( v:int     ) = let%bitstring bs = {| v:16 :           int, littleendian |} in bs::ostream
let write_int32  ostream  ( v:Int32.t ) = let%bitstring bs = {| v:32 :           int, littleendian |} in bs::ostream
let write_int64  ostream  ( v:Int64.t ) = let%bitstring bs = {| v:64 :           int, littleendian |} in bs::ostream
let write_uint8  ostream  ( v:int     ) = let%bitstring bs = {| v: 8 : unsigned, int, littleendian |} in bs::ostream
let write_uint16 ostream  ( v:int     ) = let%bitstring bs = {| v:16 : unsigned, int, littleendian |} in bs::ostream
let write_uint32 ostream  ( v:Int32.t ) = let%bitstring bs = {| v:32 : unsigned, int, littleendian |} in bs::ostream
let write_uint64 ostream  ( v:Int64.t ) = let%bitstring bs = {| v:64 : unsigned, int, littleendian |} in bs::ostream

let string_of_int8   = string_of_int 
let string_of_int16  = string_of_int 
let string_of_int32  = Int32.to_string 
let string_of_int64  = Int64.to_string 
let string_of_uint8  = string_of_int 
let string_of_uint16 = string_of_int 
let string_of_uint32 = Int32.to_string 
let string_of_uint64 = Int64.to_string 

let string_to_int8   = int_of_string 
let string_to_int16  = int_of_string 
let string_to_int32  = Int32.of_string 
let string_to_int64  = Int64.of_string 
let string_to_uint8  = int_of_string 
let string_to_uint16 = int_of_string 
let string_to_uint32 s = try Int32.of_string s with _ -> Int32.of_int (-1)
let string_to_uint64 s = try Int64.of_string s with _ -> Int64.of_int (-1)

let bpack (zero, one, double, add) blist =
    let rec mk_bin n = function
        | h::tl -> add (if h then n else zero) (mk_bin (double n) tl)
        | [] -> zero in 
        mk_bin one blist

let write_bits_int8   ostream blist = bpack (0, 1, (fun x -> 2 * x), (+)) blist |> write_int8 ostream
let write_bits_int16  ostream blist = bpack (0, 1, (fun x -> 2 * x), (+)) blist |> write_int16 ostream
let write_bits_int32  ostream blist = bpack (Int32.zero, Int32.one, Int32.mul (Int32.of_int 2), Int32.add ) blist |> write_int32 ostream
let write_bits_int64  ostream blist = bpack (Int64.zero, Int64.one, Int64.mul (Int64.of_int 2), Int64.add ) blist |> write_int64 ostream
let write_bits_uint8  ostream blist = bpack (0, 1, (fun x -> 2 * x), (+)) blist |> write_uint8 ostream
let write_bits_uint16 ostream blist = bpack (0, 1, (fun x -> 2 * x), (+)) blist |> write_uint16 ostream
let write_bits_uint32 ostream blist = bpack (Int32.zero, Int32.one, Int32.mul (Int32.of_int 2), Int32.add ) blist |> write_uint32 ostream
let write_bits_uint64 ostream blist = bpack (Int64.zero, Int64.one, Int64.mul (Int64.of_int 2), Int64.add ) blist |> write_uint64 ostream

let bit_int8   v n = (((v lsr n) land 1) = 1)
let bit_int16  v n = (((v lsr n) land 1) = 1)
let bit_int32  v n = ((Int32.logand (Int32.shift_right_logical v n) Int32.one) = Int32.one)
let bit_int64  v n = ((Int64.logand (Int64.shift_right_logical v n) Int64.one) = Int64.one)
let bit_uint8  v n = (((v lsr n) land 1) = 1)
let bit_uint16 v n = (((v lsr n) land 1) = 1)
let bit_uint32 v n = ((Int32.logand (Int32.shift_right_logical v n) Int32.one) = Int32.one)
let bit_uint64 v n = ((Int64.logand (Int64.shift_right_logical v n) Int64.one) = Int64.one)


(******************************************)
(**     Char type reading / witing       **)
(******************************************)

let read_char   bits = match%bitstring bits with {| v : 8 : int, littleendian |} -> char_of_int v , Bitstring.dropbits 8  bits 
let write_char  ostream  (v:char) = let v = int_of_char v in let%bitstring bs = {| v: 8 |} in bs::ostream  
let string_of_char = String.make 1  
let string_to_char s = String.get s 0  
let bit_char v n = bit_int8 (int_of_char v) n 
let write_bits_char ostream blist = write_bits_int8 ostream blist 


(* Utility functions *)
let nullable_to_option c ( v , bits) =
    if v = c 
        then None  , bits
        else Some v, bits

let repeat n f bits = 
    let rec loop bits = function
        | x when x < 0 -> raise (Failure "Negative type length.")
        | 0 -> [], bits
        | n -> 
            let  h, bits = f    bits in 
            let tl, bits = loop bits (n-1) in 
            h::tl, bits 
    in loop bits n
            

