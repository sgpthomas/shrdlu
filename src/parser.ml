open Model
open Grammar

let parse_command instruction = 
    let first_word = fst (String.split instruction " ") in
    let tail = snd (String.split instruction " ") in
    if first_word = "create" then create tail else
    if first_word = "delete" then delete tail else
    "output to user that they messed up";;

let create instruction = 
    let word_list = String.nsplit instruction " " in
    let _ = match word_list with 
    | [] -> 

let delete instruction = 
    "something"

let parse (s : string) =
  match s with
  | "create" -> Create (Entity (Cube, Red, []), [])
  | "print" -> Print
  | _ -> Error ("Cannot grasp meaning.")
