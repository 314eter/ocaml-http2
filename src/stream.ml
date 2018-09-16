type state =
  | Idle
  | Reserved_local
  | Reserved_remote
  | Open
  | Half_closed_local
  | Half_closed_remote
  | Closed

type action =
  | Headers
  | Push_promise
  | End_stream
  | Rst_stream

type t = {
  id : int;
  mutable state : state;
  mutable priority : Priority.t;
  mutable window : int;
}
