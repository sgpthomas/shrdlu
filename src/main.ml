open Model
open Parser
open Util


(* Welcomes the user to shrdlu *)
let welcome () =
  print_color "Welcome to Shrdlu!" AnsiDarkGrey; print_newline () ;
  print_color "Type 'q', 'quit', or 'exit' to escape." AnsiDarkGrey ; print_newline () ;
  ()

(* Prompts the user for the next input *)
let prompt () =
  let () = print_color "shrdlu> " AnsiDarkGrey in
  String.lowercase_ascii (read_line ())


(* Until the user exits, prompts the user for input and updates the model accordingly *)
let rec main (m : model) =
  let input = prompt () in
  if not (List.mem input ["q";"quit";"exit"]) then
    let Response (msg, model) = perform (parse m input) m in
    
    let () = Printf.printf "%s" msg in
    main model
  else
    () in 


(* Execute welcome and main *)
Random.self_init () ;
welcome () ;
let (m : model) = ([], []) in
main m
