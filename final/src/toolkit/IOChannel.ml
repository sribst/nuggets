

type t = {
  chan : Unix.file_descr;
  mutable in_seek: int;
  mutable out_seek: int;
}

let open_file fn = {
  chan = Unix.openfile fn [Unix.O_RDWR; Unix.O_CREAT] 0o640;
  in_seek = 0;
  out_seek = 0;
}

let close_file iochan = Unix.close iochan.chan

let seek_set chan where = Unix.lseek chan where Unix.SEEK_SET
let seek_cur chan = Unix.lseek chan 0 Unix.SEEK_CUR

let iseek_set iochan n =
  let r = seek_set iochan.chan n in
  iochan.in_seek <- r

let oseek_set iochan n =
  let r = seek_set iochan.chan n in
  iochan.out_seek <- r

let iseek_cur iochan = iochan.in_seek
let oseek_cur iochan = iochan.out_seek

let write iochan buf from n =
  oseek_set iochan iochan.out_seek;
  let r = Unix.write iochan.chan buf from n in
  iochan.out_seek <- iochan.out_seek + r; r

let read iochan buf from n =
  iseek_set iochan iochan.in_seek;
  let r = Unix.read iochan.chan buf from n in
  iochan.in_seek <- iochan.in_seek + r; r
