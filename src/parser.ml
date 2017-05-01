open Model
open Grammar
open Batteries

let string_of_leaf = function
  | Leaf(str) -> str
  | _ -> raise Tree_not_found

let rec extract_info (m : model) (instruction : tree) (address : int list) =
    let entity = at instruction address in
    let color = if ((at entity [0;1]) = (Leaf "0")) then (Leaf "colorless")
                else (at entity [0;1;0]) in
    let color = string_of_leaf color in
    let shape = string_of_leaf (at entity [0;2;0]) in
    let temp_direction = at entity [0;3] in
    let adjacent = if temp_direction = Leaf("0") then [] else
    let adj_address = if (temp_direction = Leaf("that")) then 4 else 3 in
    let direction = if ((at entity [0;adj_address;0;0]) = (Leaf "the") || (at entity [0;adj_address;0;0]) = (Leaf "0"))
                    then (at entity [0;adj_address;0;1])
                    else (at entity [0;adj_address;0;0]) in
    let direction = string_of_leaf direction in
    let (c, s, al) = extract_info m entity [0;adj_address;1] in
    let adjacent_entity = find_ID m (color_of_string c) (shape_of_string s) (List.map adjacent_of_string al) in
    [(direction,adjacent_entity)] in
    (color,shape,adjacent)

let create_command (m : model) (instruction : string) =
    let tree_list = (wrapper command instruction) in
    if List.length tree_list <> 0 then
      let tree1 = List.hd tree_list in
      let (c, s, al) = extract_info m tree1 [0;1] in
      Create (Entity (!(genid ()), (shape_of_string s), (color_of_string c)), (List.map adjacent_of_string al))
    else
      raise Tree_not_found

let delete_command (m : model) (instruction : string) =
    let tree_list = (wrapper command instruction) in
    if List.length tree_list <> 0 then
      let tree1 = List.hd tree_list in
      let (c, s, al) = extract_info m tree1 [0;1] in
      let id = find_ID m (color_of_string c) (shape_of_string s) (List.map adjacent_of_string al) in
      Delete (id)
    else
      raise Tree_not_found

let paint_command (m : model) (instruction : string) =
    let tree_list = (wrapper command instruction) in
    if List.length tree_list <> 0 then
      let tree1 = List.hd tree_list in
      let (c, s, al) = extract_info m tree1 [0;1] in
      let id = find_ID m (color_of_string c) (shape_of_string s) (List.map adjacent_of_string al) in
      let new_color = string_of_leaf (at tree1 [0;2;0]) in
      Paint (id, color_of_string new_color)
    else
      raise Tree_not_found

(* let move_command (m : model) instruction =
    let tree_list = (wrapper command instruction) in
    if List.length tree_list <> 0 then
      let tree1 = List.hd tree_list in
      let (c, s, al) = extract_info m tree1 [0;1] in
      let id = find_ID m (color_of_string c) (shape_of_string s) (List.map adjacent_of_string al) in
      let new_color = at tree1 [0;2;1;0] in
      Paint (id, color)
    else
      raise Tree_not_found *)

let parse (m : model) (instruction : string) =
  if instruction = "" then Error ("type something") else
    let first_word = List.hd (String.split_on_char ' ' instruction) in
    try
      match first_word with
      | "create" -> create_command m instruction
      | "delete" -> delete_command m instruction
      | "paint" -> paint_command m instruction
      (* | "move" -> move_command m instruction *)
      | "#print" -> Print
      | "#reset" -> Reset
      | _ -> Error ("Unable to grasp meaning")
    with
    | No_such_adjacent_exception -> Error ("No such adjacent")
    | No_such_direction_exception -> Error ("No such direction")
    | No_such_shape_exception -> Error ("No such shape")
    | No_such_color_exception -> Error ("No such color")
    | No_such_entity_exception (msg) -> Error (Printf.sprintf "Unable to find this '%s' in the model" msg)
    | Tree_not_found -> Error ("Failed to parse input")
    (* | _ -> Error ("Something unexpected went wrong") *)

(* let parse (s : string) =
  match s with
  | "#create 1" -> Create (Entity (!(genid ()), Cube, Red), [])
  | "#create 2" -> Create (Entity (!(genid ()), Sphere, Blue), [Adjacent (Right, 1)])
  | "#create 3" -> Create (Entity (!(genid ()), Pyramid, Yellow), [Adjacent (Front, 2)])
  | "#create 4" -> Create (Entity (!(genid ()), Sphere, Orange), [Adjacent (Front, 1)])
  | "#find 1" -> Find (Red, Cube, [Adjacent (Right, 2)])
  | "#find 2" -> Find (Blue, Sphere, [Adjacent (Left, 1) ; Adjacent (Front, 3)])
  | "#find 3" -> Find (Yellow, Pyramid, [Adjacent (Behind, 2)])
  | "#find 4" -> Find (Orange, Sphere, [Adjacent (Behind, 1)])
  | "#find 5" -> Find (Red, Cube, [Adjacent (Right, 2) ; Adjacent (Behind, 4)])
  | "#print" -> Print
  | _ -> Error ("Cannot grasp meaning.") *)
