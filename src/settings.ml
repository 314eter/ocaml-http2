open S

type identifier = int
type value = int

type settings = int array

let header_table_size = 0x1
let enable_push = 0x2
let max_concurrent_streams = 0x3
let initial_window_size = 0x4
let max_frame_size = 0x5
let max_header_list_size = 0x6

let identifier_of_int id = id

let set settings id value =
  match id with
  | 0x2 when value > 1 -> raise (HTTP2Error Protocol_error)
  | 0x4 when value > 0x7fffffff -> raise (HTTP2Error Flow_control_error)
  | 0x5 when value < 0x4000 || value > 0xffffff -> raise (HTTP2Error Protocol_error)
  | _ -> if id > 0 && id < 7 then settings.(id) <- value
