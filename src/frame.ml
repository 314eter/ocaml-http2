open S

type typ =
  | Data
  | Headers
  | Priority
  | Rst_stream
  | Settings
  | Push_promise
  | Ping
  | Goaway
  | Window_update
  | Continuation
  | Unknown

type frame_header = {
  typ : typ;
  length : int;
  flags : int;
  stream_identifier : int;
}

let typ_of_int = function
  | 0 -> Data
  | 1 -> Headers
  | 2 -> Priority
  | 3 -> Rst_stream
  | 4 -> Settings
  | 5 -> Push_promise
  | 6 -> Ping
  | 7 -> Goaway
  | 8 -> Window_update
  | 9 -> Continuation
  | _ -> Unknown

let int_of_typ = function
  | Data -> 0
  | Headers -> 1
  | Priority -> 2
  | Rst_stream -> 3
  | Settings -> 4
  | Push_promise -> 5
  | Ping -> 6
  | Goaway -> 7
  | Window_update -> 8
  | Continuation -> 9
  | Unknown -> assert false

module Reader (C : C) (IO : IO with type 'a t = 'a C.t) = struct
  module Util = Util.Make (C)
  open Util

  let read_byte ic =
      match%bind IO.read_byte ic with
      | Some b -> R.return b
      | None -> R.fail End_of_file

    let read_string ic length =
      match%bind IO.read_string ic length with
      | Some s -> R.return s
      | None -> R.fail End_of_file

  let read_int16 ic =
    let%bind.R b0 = read_byte ic in
    let%bind.R b1 = read_byte ic in
    R.return ((b0 lsl 8) lor b1)

  let read_int24 ic =
    let%bind.R b0 = read_byte ic in
    let%bind.R b1 = read_byte ic in
    let%bind.R b2 = read_byte ic in
    R.return ((b0 lsl 16) lor (b1 lsl 8) lor b2)

  let read_int32 ic =
    let%bind.R b0 = read_byte ic in
    let%bind.R b1 = read_byte ic in
    let%bind.R b2 = read_byte ic in
    let%bind.R b3 = read_byte ic in
    R.return ((b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3)

  let read_bool_int31 ic =
    let%bind.R i = read_int32 ic in
    R.return (i >= 0x80000000, i land 0x7fffffff)

  let read_length_padding ic length flags =
    if Flags.get flags Flags.padded then begin
      if length < 1 then raise (HTTP2Error Protocol_error);
      let%bind.R padding = read_byte ic in
      R.return (length - padding - 1, padding)
    end else R.return (length, 0)

  let rec skip_padding ic = function
    | 0 -> R.return ()
    | length ->
      let%bind.R b = read_byte ic in
      if b != 0 then raise (HTTP2Error Protocol_error);
      skip_padding ic (length - 1)

  let read_data ic length flags f =
    let%bind.R (length, padding) = read_length_padding ic length flags in
    if length < 0 then raise (HTTP2Error Protocol_error);
    let%bind () = f length in
    skip_padding ic padding

  let _read_priority ic =
    let%bind.R (exclusive, stream_dependency) = read_bool_int31 ic in
    let%bind.R b = read_byte ic in
    R.return Priority.{exclusive; stream_dependency; weight = b + 1}

  let read_length_priority ic length flags =
    if Flags.get flags Flags.priority then begin
      if length < 5 then raise (HTTP2Error Protocol_error);
      let%bind priority = _read_priority ic in
      R.return (length - 5, Some priority)
    end else R.return (length, None)

  let read_headers ic length flags f =
    let%bind.R (length, padding) = read_length_padding ic length flags in
    let%bind.R (length, priority) = read_length_priority ic length flags in
    if length < 0 then raise (HTTP2Error Protocol_error);
    let%bind () = f length in
    let%bind.R () = skip_padding ic padding in
    R.return priority

  let read_priority ic length =
    if length != 5 then raise (HTTP2Error Frame_size_error);
    _read_priority ic

  let read_rst_stream ic length =
    if length != 4 then raise (HTTP2Error Frame_size_error);
    let%bind.R error_code = read_int32 ic in
    R.return (error_of_int error_code)

  let read_settings ic length flags =
    if Flags.get flags Flags.ack then
      if length != 0 then raise (HTTP2Error Frame_size_error)
      else R.return None
    else if length mod 6 != 0 then raise (HTTP2Error Frame_size_error) else
      let rec loop = function
        | 0 -> R.return []
        | length ->
          let%bind.R identifier = read_int16 ic in
          let%bind.R value = read_int32 ic in
          let%bind.R settings = loop (length - 6) in
          R.return @@ (identifier, value) :: settings in
      let%bind.R settings = loop length in
      R.return (Some settings)

  let read_push_promise ic length flags f =
    let%bind.R (length, padding) = read_length_padding ic length flags in
    if length < 1 then raise (HTTP2Error Protocol_error);
    let%bind.R (_, promised_stream_id) = read_bool_int31 ic in
    let%bind () = f length in
    let%bind.R () = skip_padding ic padding in
    R.return promised_stream_id

  let read_ping ic length =
    if length != 8 then raise (HTTP2Error Frame_size_error);
    read_string ic 8

  let read_goaway ic length f =
    if length < 8 then raise (HTTP2Error Frame_size_error);
    let%bind.R (_, last_stream_id) = read_bool_int31 ic in
    let%bind.R error_code = read_int32 ic in
    let%bind () = f (length - 8) in
    R.return (last_stream_id, error_code)

  let read_window_update ic length =
    if length != 4 then raise (HTTP2Error Frame_size_error);
    let%bind.R (_, increment) = read_bool_int31 ic in
    R.return increment

  let read_frame_header ic =
    let%bind.R length = read_int24 ic in
    let%bind.R t = read_byte ic in
    let%bind.R flags = read_byte ic in
    let%bind.R (_, stream_identifier) = read_bool_int31 ic in
    R.return {typ = typ_of_int t; length; flags; stream_identifier}
end

module Writer (C : C) (IO : IO with type 'a t = 'a C.t) = struct
  module Util = Util.Make (C)
  open Util

  let write_int16 oc i =
    let%bind () = IO.write_byte oc ((i lsr 8) land 255) in
    IO.write_byte oc (i land 255)

  let write_int24 oc i =
    let%bind () = IO.write_byte oc ((i lsr 16) land 255) in
    let%bind () = IO.write_byte oc ((i lsr 8) land 255) in
    IO.write_byte oc (i land 255)

  let write_int32 oc i =
    let%bind () = IO.write_byte oc ((i lsr 24) land 255) in
    let%bind () = IO.write_byte oc ((i lsr 16) land 255) in
    let%bind () = IO.write_byte oc ((i lsr 8) land 255) in
    IO.write_byte oc (i land 255)

  let write_bool_int31 oc b i =
    write_int32 oc (if b then i lor 128 else i land 127)

  let write_data length f =
    f length

  let write_headers length f =
    f length

  let write_frame_header oc {typ; length; flags; stream_identifier} =
    let%bind () = write_int24 oc length in
    let%bind () = IO.write_byte oc (int_of_typ typ) in
    let%bind () = IO.write_byte oc flags in
    write_bool_int31 oc false stream_identifier
end
