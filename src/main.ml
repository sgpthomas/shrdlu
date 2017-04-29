open Model
open Parser

let prompt () =
  let () = print_string "shrdlu> " in
  read_line ()

let print_line s =
  if s = "" then () else (print_string s ; print_newline ())

let main (m : model) =
  let input = prompt () in
  if not (List.mem input ["q";"quit";"exit"]) then
    let Response (msg, model) = perform (parse input) m in
    let () = print_line msg in
    (true, model)
  else
    (false, m)

let rec loop (m : model) =
  match (main m) with
  | (true, n) -> loop n
  | (false, _) -> ()

let welcome () =
    print_line "Welcome to Shrdlu!" ;
    print_line "Type 'q', 'quit', or 'exit' to escape." ;
    ()
;;

welcome ();;
let (m : model) = [] in
loop m;;

