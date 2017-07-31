let print_file file =
  let chan = open_in file in
  try while true do print_endline (input_line chan) done with
  | End_of_file -> close_in chan

let get_filename file =
  let idx = String.rindex file '/' in
  let idx = succ idx in
  String.sub file idx (String.length file - idx)

let () =
  for i = 1 to pred (Array.length Sys.argv) do
    let file = Sys.argv.(i) in
    let filename = get_filename file in
    let modul = String.capitalize filename in
    print_endline ("module " ^ modul ^ " : sig");
    print_file (file ^ ".mli");
    print_endline "end = struct";
    print_file (file ^ ".ml");
    print_endline "end";
    print_newline ();
  done;
  print_file "bootstrap.ml"
