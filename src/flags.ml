let end_stream = 0x1
let ack = 0x1
let end_headers = 0x4
let padded = 0x8
let priority = 0x20

let get flags flag =
  flags land flag = flag

let set flags flag =
  flags lor flag

let unset flags flag =
  flags land (lnot flag)
