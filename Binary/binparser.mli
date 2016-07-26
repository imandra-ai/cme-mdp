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
val metadata_to_string: msg_metadata -> string

val string_to_pt : string -> primitive_type 
val pt_to_string : primitive_type -> string
val to_ocaml : primitive_type -> string


type pcap_info = {
    src_mac : string;
    dst_mac : string;
    src_ip  : string;
    dst_ip  : string;
    src_port: string;
    dst_port: string
}


type in_t  
type out_t 

val open_in  : string -> in_t 
val create_out : unit -> out_t
val write_out : string -> out_t -> unit
val concat_out : out_t list -> out_t
val out_to_in : out_t -> in_t
val length : in_t -> int
val hexdump: in_t -> unit
val hexdump_o: out_t -> unit

val read_pcap_file_info: ?vrb:bool ->  in_t -> in_t 
val write_pcap_file_info: unit -> out_t
val get_pcap_message: ?vrb:bool -> in_t -> msg_metadata * in_t * in_t
val make_pcap_header: pcap_info -> int -> out_t -> out_t
val get_message :     ?vrb:bool -> in_t -> msg_metadata * in_t * in_t 
val write_message: out_t -> msg_metadata * out_t * out_t -> out_t
val skip: in_t -> int -> in_t
val   get_group_info :  in_t -> int * int *  in_t 
val write_group_info : out_t -> int * int -> out_t
val cut_block : in_t -> int -> in_t * in_t
val append_padded : out_t -> out_t -> int -> out_t

val read_int8   : in_t -> int   * in_t 
val read_int16  : in_t -> int   * in_t 
val read_int32  : in_t -> int32 * in_t 
val read_int64  : in_t -> int64 * in_t 
val read_uint8  : in_t -> int   * in_t 
val read_uint16 : in_t -> int   * in_t 
val read_uint32 : in_t -> int32 * in_t 
val read_uint64 : in_t -> int64 * in_t 
val read_char   : in_t -> char  * in_t 

val write_int8   : out_t -> int   -> out_t 
val write_int16  : out_t -> int   -> out_t 
val write_int32  : out_t -> int32 -> out_t 
val write_int64  : out_t -> int64 -> out_t 
val write_uint8  : out_t -> int   -> out_t 
val write_uint16 : out_t -> int   -> out_t 
val write_uint32 : out_t -> int32 -> out_t 
val write_uint64 : out_t -> int64 -> out_t 
val write_char   : out_t -> char  -> out_t 

val write_bits_int8   : out_t -> bool list -> out_t 
val write_bits_int16  : out_t -> bool list -> out_t 
val write_bits_int32  : out_t -> bool list -> out_t 
val write_bits_int64  : out_t -> bool list -> out_t 
val write_bits_uint8  : out_t -> bool list -> out_t 
val write_bits_uint16 : out_t -> bool list -> out_t 
val write_bits_uint32 : out_t -> bool list -> out_t 
val write_bits_uint64 : out_t -> bool list -> out_t 
val write_bits_char   : out_t -> bool list -> out_t 


val bit_int8   : int   -> int -> bool 
val bit_int16  : int   -> int -> bool 
val bit_int32  : int32 -> int -> bool 
val bit_int64  : int64 -> int -> bool 
val bit_uint8  : int   -> int -> bool 
val bit_uint16 : int   -> int -> bool 
val bit_uint32 : int32 -> int -> bool 
val bit_uint64 : int64 -> int -> bool 
val bit_char   : char  -> int -> bool 


val string_of_int8   : int   -> string 
val string_of_int16  : int   -> string 
val string_of_int32  : int32 -> string 
val string_of_int64  : int64 -> string 
val string_of_uint8  : int   -> string 
val string_of_uint16 : int   -> string 
val string_of_uint32 : int32 -> string 
val string_of_uint64 : int64 -> string 
val string_of_char   : char  -> string 

val string_to_int8   : string -> int   
val string_to_int16  : string -> int   
val string_to_int32  : string -> int32 
val string_to_int64  : string -> int64 
val string_to_uint8  : string -> int   
val string_to_uint16 : string -> int   
val string_to_uint32 : string -> int32 
val string_to_uint64 : string -> int64 
val string_to_char   : string -> char  

(* Utility functions *)
val nullable_to_option: 'a -> 'a * in_t -> 'a option * in_t
val repeat: int -> ( in_t -> 'a * in_t ) -> in_t -> 'a list * in_t 

