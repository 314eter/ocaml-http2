module type C = sig
  type +'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

module type IO = sig
  type +'a t

  type ic
  type oc

  val read_byte : ic -> int option t
  val write_byte : oc -> int -> unit t

  val read_string : ic -> int -> string option t
  val write_string : oc -> string -> unit t

  val read_into : ic -> bytes -> int -> int -> unit t
  val write_from : oc -> string -> int -> int -> unit t
end

type error =
  | No_error
  | Protocol_error
  | Internal_error
  | Flow_control_error
  | Settings_timeout
  | Stream_closed
  | Frame_size_error
  | Refused_stream
  | Cancel
  | Compression_error
  | Connect_error
  | Enhance_your_calm
  | Inadequate_security
  | Http_1_1_required

exception HTTP2Error of error

let error_of_int = function
  | 0 -> No_error
  | 1 -> Protocol_error
  | 2 -> Internal_error
  | 3 -> Flow_control_error
  | 4 -> Settings_timeout
  | 5 -> Stream_closed
  | 6 -> Frame_size_error
  | 7 -> Refused_stream
  | 8 -> Cancel
  | 9 -> Compression_error
  | 10 -> Connect_error
  | 11 -> Enhance_your_calm
  | 12 -> Inadequate_security
  | 13 -> Http_1_1_required
  | _ -> Internal_error

type header = Hpack.header = {
  name : string;
  value : string;
  never_index : bool;
}
