open S

module Make (C : C) (IO : IO with type 'a t = 'a C.t) = struct
  module Util = Util.Make (C)
  open Util

  type +'a t = 'a C.t

  type 'a wrapper = {
    channel : 'a;
    mutable left : int;
    mutable ended : bool;
    callback : unit -> (unit, exn) result C.t;
  }

  type ic = IO.ic wrapper
  type oc = IO.oc wrapper

  let read_byte ic =
    let rec loop () =
      if ic.left = 0 then
        if ic.ended then
          C.return None
        else
          match%bind ic.callback () with
          | Ok () -> loop ()
          | _ -> C.return None
      else begin
        ic.left <- ic.left - 1;
        IO.read_byte ic.channel
      end in
    loop ()

  let write_byte oc b =
    let rec loop () =
      if oc.left = 0 then
        match%bind oc.callback () with
        | Ok () -> loop ()
        | _ -> C.return ()
      else begin
        oc.left <- oc.left - 1;
        IO.write_byte oc.channel b
      end in
    loop ()

  let read_string ic length =
    let buffer = Bytes.create length in
    let rec loop offset =
      if offset < length then
        if ic.left = 0 then
          if ic.ended then
            C.return None
          else
            match%bind ic.callback () with
            | Ok () -> loop offset
            | _ -> C.return None
        else
            let l = min ic.left (length - offset) in
            ic.left <- ic.left - l;
            let%bind () = IO.read_into ic.channel buffer offset l in
            loop (offset + l)
      else
        C.return @@ Some (Bytes.unsafe_to_string buffer) in
    loop 0

  let write_string oc s =
    let length = String.length s in
    let rec loop offset =
      if offset < length then
        if oc.left = 0 then
          match%bind oc.callback () with
          | Ok () -> loop offset
          | _ -> C.return ()
        else
          let l = min oc.left (length - offset) in
          oc.left <- oc.left - l;
          let%bind () = IO.write_from oc.channel s offset l in
          loop (offset + l)
      else C.return () in
    loop 0
end
