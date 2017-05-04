open Util
open Model
open Grammar
open Batteries

(* Returns the string of a leaf *)
let string_of_leaf = function
  | Leaf(str) -> str
  | _ -> raise Not_a_leaf


(* Generates a random color string *)
let random_color () =
  let color_list = ["red";"orange";"yellow";"green";"blue";"purple"] in
  let random = Random.int (List.length color_list) in
  List.nth color_list random

(* Generates a random shape string *)
let random_shape () =
  let shape_list = ["cube";"sphere";"pyramid"] in
  let random = Random.int (List.length shape_list) in
  List.nth shape_list random


(* Ensures that proper determiners are being used *)
let check_det command_type tree =
  let find_det tree address =
    if (at tree address) = (Leaf "0") then "a" else (* Null determiner equivalent to "a" *)
    let address = (List.append address [0]) in 
    let maybe_det = at tree address in
      match maybe_det with 
      | Branch("Number",_) -> string_of_leaf (at maybe_det [0])
      | _ -> string_of_leaf maybe_det
  in
  match  command_type with
  | "adjacent" -> let det = find_det tree [0] in 
                  if (det = "the" || det = "a") then true
                  else false
  | "create" -> let det = find_det tree [0;1;0] in
                if (det = "the" || det = "all") then false
                else true
  | "delete" -> true
  | "paint"  -> true
  | "move"   -> let det = find_det tree [0;3;0] in
                if (det = "a" || det = "the") then true
                else false
  | _ -> raise No_such_command_exception


(* The most important function in parser. Given the address of an entity
 * a tree, it will extract its color, shape, and adjacent entities. Works
 * recursively by also extracting the info of the adjacent entities.
 *)
let rec extract_info (m : model) (instruction : tree) (address : int list) =
  let entity = at instruction address in
  let color = string_of_leaf (
    if ((at entity [1;0]) = (Leaf "0")) then
      match (at instruction [0]) with
        (* If color not defined for create command, default to white *)
        | Branch ("Create", _) -> (Leaf (random_color ()))
        (* If color not defined for other commands, then color unknown *)
        | _ -> (Leaf "unknown")
    (* Color has been explicitly defined *)
    else (at entity [1;0;0])
  ) in
  let shape = string_of_leaf (at entity [1;1;0]) in
  let maybe_direction = at entity [1;2] in
  let adjacent = 
    (* No adjacent entities *)
    if (maybe_direction = Leaf("0")) then [] 
    else let adj_address = if (maybe_direction = Leaf("that")) then 3 else 2 in
    if not (check_det "adjacent" (at entity [1;adj_address;1])) then raise Incorrect_determiner else
    let direction = 
      string_of_leaf (
        if ((at entity [1;adj_address;0;0]) = (Leaf "the") || (at entity [1;adj_address;0;0]) = (Leaf "0"))
        then (at entity [1;adj_address;0;1])
        else (at entity [1;adj_address;0;0]) 
      ) in
    let (c, s, al) = extract_info m entity [1;adj_address;1] in
    let adjacent_entity = find_ID m c s al in
  [direction, adjacent_entity] in
  (color_of_string color, shape_of_string shape, (List.map adjacent_of_string adjacent))


(* Brackets the sentence into its top level constituents for disambigatuion purposes *)
let bracket_tree (tr : tree) =
  let rec get_all_leaves (sl : string list) (t : tree) =
  match t with
  | Leaf (s) -> List.append sl [s]
  | Branch (s, tree_list) -> List.flatten (List.map (get_all_leaves sl) tree_list)
  in
  let rec string_of_list = function
  | [] -> ""
  | hd::tl -> hd ^ " " ^ (string_of_list tl)
  in
  match at tr [0] with
  | Leaf (s) -> Printf.sprintf "( %s )" s
  | Branch (s, tree_list) ->
    String.filter (fun x -> x <> '0') 
    (string_of_list 
      (List.map (fun l -> "( " ^ string_of_list l ^ ")") 
        (List.map (fun t -> (get_all_leaves []) t) tree_list)
      )
    )


(* Presents the user with bracketed sentences and asks them to choose one *)
let resolve_ambiguity (tl : tree list) =
  let sz = List.length tl in
  if sz = 1 then
  List.hd tl
  else
  begin
    List.iteri (fun i t -> Printf.printf " [%d] -> %s\n" i (bracket_tree t)) tl ;
    let rec p () =
    Printf.printf "enter 0-%d to choose:\n -> " (sz-1) ;
    try
      let choice = read_int () in List.nth tl (choice)
    with
    | _ -> p ()
    in
    p ()
  end


(* Given an instruction, returns a single parse tree or raises Tree_not_found *)
let get_tree (instruction : string) = 
  let tree_list = (wrapper command instruction) in
  if List.length tree_list <> 0 then resolve_ambiguity tree_list
  else raise Tree_not_found


(* Returns a create command given a string and model *)
let create_command (m : model) (instruction : string) =
  let tree = get_tree instruction in
  if not (check_det "create" tree) then raise Incorrect_determiner else
    let (c, s, al) = extract_info m tree [0;1] in
    let s = if s = Object then (shape_of_string (random_shape ())) else s in 
    Create (Entity (!(genid ()), s, c), al)


(* Returns a delete command given a string and model *)
let delete_command (m : model) (instruction : string) =
  let tree = get_tree instruction in
  if not (check_det "delete" tree) then raise Incorrect_determiner else
    let (c, s, al) = extract_info m tree [0;1] in
    let id = find_ID m c s al in
    Delete (id)


(* Returns a paint command given a string and model *)
let paint_command (m : model) (instruction : string) =
  let tree = get_tree instruction in
  if not (check_det "paint" tree) then raise Incorrect_determiner else
    let (c, s, al) = extract_info m tree [0;1] in
    let id = find_ID m c s al in
    let new_color = string_of_leaf (at tree [0;2;0]) in
    Paint (id, color_of_string new_color)


(* Returns a move command given a string and model *)
let move_command (m : model) (instruction : string) =
  let tree = get_tree instruction in
  if not (check_det "move" tree) then raise Incorrect_determiner else
    let (c, s, al) = extract_info m tree [0;1] in
    let id = find_ID m c s al in
    let (c2, s2, al2) = extract_info m tree [0;3] in
    let id2 = find_ID m c2 s2 al2 in
    let direction = string_of_leaf (
      if ((at tree [0;2;0]) = (Leaf "the") || (at tree [0;2;0]) = (Leaf "0"))
      then (at tree [0;2;1])
      else (at tree [0;2;0]) 
    ) in
    Move (id, [Adjacent (direction_of_string direction, id2)])


(* Returns an exists command given a string and model *)
let exists_command (m : model) (instruction : string) =
  let tree = get_tree instruction in
    let quantifier = quantifier_of_string (string_of_leaf (at tree [0;0;1;0]))
    (int_of_string (string_of_leaf (at tree [0;0;1;1;0]))) in
    let (c, s, al) = extract_info m tree [0;0;2] in
    Exist (quantifier, (c, s,al))


(* Identifies and executes the instruction based on the first word of the instruction *)
let parse (m : model) (instruction : string) =
  if instruction = "" then Error ("type something") else
  let first_word = List.hd (String.split_on_char ' ' instruction) in
  try
    match first_word with
    | "create" -> create_command m instruction
    | "delete" -> delete_command m instruction
    | "paint" -> paint_command m instruction
    | "move" -> move_command m instruction
    | "are"
    | "is" -> exists_command m instruction
    | "#print" -> Print
    | "#reset" -> Reset
    | "#parse" -> List.iteri (writetree) (wrapper command (snd (String.split instruction " "))) ; Error ("parsed tree")
    | _ -> Error ("Unable to grasp meaning")
  with
  | No_such_adjacent_exception -> Error ("No such adjacent")
  | No_such_direction_exception -> Error ("No such direction")
  | No_such_shape_exception -> Error ("No such shape")
  | No_such_color_exception -> Error ("No such color")
  | No_such_command_exception -> Error ("No such command")
  | No_such_entity_exception (msg) -> Error (Printf.sprintf "Unable to find this '%s' in the model" msg)
  | Tree_not_found -> Error ("Failed to parse input")
  | Not_a_leaf -> Error ("Called string_of_leaf incorrectly")
  | Incorrect_determiner -> Error ("Used an incorrect determiner")
  | _ -> Error ("Something unexpected went wrong")
