
type content = {
  section  : string;
  display  : bool;
  write    : bool;
  mutable messages : string list;
}

module Log = Map.Make (struct
  type t = int
  let compare = ( - )
end)

module Section = Hashtbl.Make (struct
  type t = string
  let equal = String.equal
  let hash = Hashtbl.hash
end)

let logs : content Log.t ref = ref Log.empty
let sections : int Section.t = Section.create 10

let fresh_id = let r = ref 0 in fun () -> incr r; !r

let new_section ~name ?(display = false) ?(write = true) () =
  let id = fresh_id () in
  Section.replace sections name id;
  logs := Log.add id {section = name; display; write; messages = []} !logs 

let log ~section ~message =
  let id = try Section.find sections section with _ -> assert false in
  let current_content = Log.find id !logs in
  current_content.messages <- message :: current_content.messages;
  if current_content.display then
    Printf.printf "[%s] %s\n%!" section message

let write_content ochan content =
  let s = Printf.sprintf "[%s]\n" content.section in
  let s = List.fold_right (fun msg s -> Printf.sprintf "%s%s\n" s msg)
    content.messages s in
  output_string ochan s

let write_log ~filename =
  let chan = open_out filename in
  Log.iter (fun _ content ->
    if content.write then write_content chan content
  ) !logs;
  close_out chan

let () = new_section
  ~name:"DEBUG"
  ~display:true
  ~write:false ()

let debug message = log ~section:"DEBUG" ~message
