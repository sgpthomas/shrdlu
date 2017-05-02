open Model
open Parser
open Util

let prompt () =
  let () = print_color "shrdlu> " AnsiDarkGrey in
  String.lowercase_ascii (read_line ())

let main (m : model) =
  let input = prompt () in
  if not (List.mem input ["q";"quit";"exit"]) then
    let Response (msg, model) = perform (parse m input) m in
    let () = Printf.printf "%s" msg in
    (true, model)
  else
    (false, m)

let rec loop (m : model) =
  match (main m) with
  | (true, n) -> loop n
  | (false, _) -> ()

let welcome () =
    print_color "Welcome to Shrdlu!" AnsiDarkGrey; print_newline () ;
    print_color "Type 'q', 'quit', or 'exit' to escape." AnsiDarkGrey ; print_newline () ;
    ()
;;

welcome ();;
let (m : model) = ([], []) in
loop m;;

