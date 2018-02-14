
type chan = {
  data     : IOChannel.t;
  big_data : IOChannel.t;
}

let openfiles ~data ~big_data = {
  data = IOChannel.open_file data;
  big_data = IOChannel.open_file big_data;
}

let closefiles chan =
  IOChannel.close_file chan.data;
  IOChannel.close_file chan.big_data

module type I = sig
  type key
  val to_position : key -> int
  type value
  val serialize   : value -> bytes
  val deserialize : bytes -> value
  val max_byte : int
end

module type S = sig
  type key
  type value
  val get : chan -> key -> value
  val set : chan -> key -> value -> unit
end

module Make (KV:I) = struct 

  type key = KV.key
  type value = KV.value

  let set chan k v =
    let max = KV.max_byte in
    let pos = (KV.to_position k ) * max in
    IOChannel.oseek_set chan.data pos;
    let v_s = KV.serialize v in
    try
      let v_s = Serial.fill max v_s in
      ignore (IOChannel.write chan.data v_s 0 max)
    with Serial.BiggerThanExpected ->
      let offset = IOChannel.oseek_cur chan.big_data in
      let off_s  = Serial.(flag_last (fill max (serialize offset))) in
      ignore (IOChannel.write chan.data off_s 0 max);
      let v_s = Serial.flag_with_size v_s in
      let size = String.length v_s in
      ignore (IOChannel.write chan.big_data v_s 0 size)

  let get chan k =
    let max = KV.max_byte in 
    let pos = (KV.to_position k ) * max in
    IOChannel.iseek_set chan.data pos;
    let v_s = Bytes.create max in
    ignore (IOChannel.read chan.data v_s 0 max);
    if not (Serial.is_flagged_last v_s) then
      let v_s = Serial.unfill v_s in
      KV.deserialize v_s
    else
      let v_s = Serial.unflag_last v_s    in
      let offset = Serial.deserialize v_s in
      IOChannel.iseek_set chan.big_data offset;
      let b_size = Bytes.create 1 in
      ignore (IOChannel.read chan.big_data b_size 0 1);
      let b_size = String.get b_size 0 in
      let size = Serial.size_flag b_size in
      let v_s = Bytes.create size in
      ignore (IOChannel.read chan.big_data v_s 0 size);  
      KV.deserialize v_s

end
