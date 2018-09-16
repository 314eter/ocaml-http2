type t = {
  exclusive : bool;
  stream_dependency : int;
  weight : int;
}

let default = {
  exclusive = false;
  stream_dependency = 0;
  weight = 1;
}
