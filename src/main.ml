
let prompt () =
  let () = print_string "shrdlu> " in
  read_line ()
;;

let print_line s =
  print_string s ; print_newline ()
;;

let rec main () =
  let response = prompt () in
  if not (List.mem response ["q";"quit";"exit"]) then
    let () = print_line response in
    1
  else
    0
;;

let rec loop () =
  if main () = 1 then loop () else ()
;;

let welcome () =
    print_line "Welcome to Shrdlu!" ;
    print_line "Type 'q', 'quit', or 'exit' to escape." ;
    ()
;;

welcome ();;
loop ();;
