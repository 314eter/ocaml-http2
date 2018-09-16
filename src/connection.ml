open S

module Streams = Hashtbl.MakeSeeded (struct
    type t = int
    let equal (x : int) (y : int) = x = y
    let hash = Hashtbl.seeded_hash
  end)

type event =
  | Header of header
  | Data of int
  | Goaway of int

module Make (C : C) (IO : IO with type 'a t = 'a C.t) = struct
  module Util = Util.Make (C)
  open Util

  module FrameReader = Frame.Reader (C) (IO)
  module FrameWriter = Frame.Writer (C) (IO)
  module Hpack_io = Hpack_io.Make (C) (IO)
  module Hpack_decoder = Hpack.Decoder.Make (C) (Hpack_io)

  type t = {
    streams : Stream.t Streams.t;
    ic : IO.ic;
    oc : IO.oc;
    hpack_decoder : Hpack.Decoder.t;
    hpack_encoder : Hpack.Encoder.t;
    mutable window : int;
  }

  let create () = assert false

  let create_hpack_ic ic flags stream_identifier =
    let rec hpack_ic = Hpack_io.{channel = ic; left = 0; ended = Flags.get flags Flags.end_headers; callback}
    and callback () =
      let%bind.R {typ; length; flags; stream_identifier = stream_identifier'} = FrameReader.read_frame_header ic in
      assert (typ = Continuation && stream_identifier = stream_identifier');
      hpack_ic.ended <- Flags.get flags Flags.end_headers;
      hpack_ic.left <- length;
      R.return () in
    hpack_ic

  let receive {streams; ic; hpack_decoder; _} callback =
    let%bind.R {typ; length; flags; stream_identifier} = FrameReader.read_frame_header ic in
    let stream =
      match Streams.find_opt streams stream_identifier with
      | Some stream -> stream
      | None ->
        let stream = Stream.{id = stream_identifier; state = Idle; priority = Priority.default; window = 65535} in
        Streams.add streams stream_identifier stream;
        stream in
    match stream.state, typ with
    | _, Priority ->
      let%bind.R priority = FrameReader.read_priority ic length in
      R.return ()
    | _, Ping ->
      let%bind.R ping = FrameReader.read_ping ic length in
      R.return ()
    | _, Goaway ->
      let%bind.R (last_stream_id, error_code) = FrameReader.read_goaway ic length (fun length -> callback (Goaway length)) in
      R.return ()
    | _, Window_update ->
      let%bind.R increment = FrameReader.read_window_update ic length in
      stream.window <- stream.window + increment;
      R.return ()
    | (Open | Half_closed_local), Data ->
      FrameReader.read_data ic length flags (fun length -> callback (Data length))
    | Idle, Headers ->
      let hpack_ic = create_hpack_ic ic flags stream_identifier in
      let%bind.R priority = FrameReader.read_headers ic length flags (fun length -> hpack_ic.left <- length; C.return ()) in
      let%bind.R () = Hpack_decoder.decode_headers hpack_decoder hpack_ic (fun header -> callback (Header header)) in
      stream.state <- if Flags.get flags Flags.end_stream then Half_closed_remote else Open;
      R.return ()
    | Idle, Push_promise ->
      let hpack_ic = create_hpack_ic ic flags stream_identifier in
      let%bind.R promised_stream_id = FrameReader.read_push_promise ic length flags (fun length -> hpack_ic.left <- length; C.return ()) in
      let%bind.R () = Hpack_decoder.decode_headers hpack_decoder hpack_ic (fun header -> callback (Header header)) in
      stream.state <- Reserved_remote;
      R.return ()
    | Idle, _ -> assert false
    | _, Rst_stream ->
      let%bind.R error = FrameReader.read_rst_stream ic length in
      stream.state <- Closed;
      R.return ()
    | Reserved_remote, Headers ->
      let hpack_ic = create_hpack_ic ic flags stream_identifier in
      let%bind.R priority = FrameReader.read_headers ic length flags (fun length -> hpack_ic.left <- length; C.return ()) in
      let%bind.R () = Hpack_decoder.decode_headers hpack_decoder hpack_ic (fun header -> callback (Header header)) in
      stream.state <- (if Flags.get flags Flags.end_stream then Closed else Half_closed_local);
      R.return ()
    | _ -> assert false
end
