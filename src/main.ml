let prompt () =
  let () = print_string "shrdlu> " in
  read_line ()
;;

let print_line s =
  print_string s ; print_newline ()
;;

let main (m : Model.model) =
  let input = prompt () in
  if not (List.mem input ["q";"quit";"exit"]) then
    let () = print_line (Model.perform (Parser.parse input) m) in
    (true, m)
  else
    (false, m)
;;

let rec loop (m : Model.model) =
  match (main m) with
  | (true, n) -> loop n
  | (false, _) -> ()
;;

let welcome () =
    print_line "Welcome to Shrdlu!" ;
    print_line "Type 'q', 'quit', or 'exit' to escape." ;
    ()
;;

welcome ();;
let (m : Model.model) = [] in
loop m;;

