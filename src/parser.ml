open Model
open Grammar
open Batteries

let string_of_leaf = function
  | Leaf(str) -> str
  | _ -> raise Tree_not_found

let rec extract_info (m : model) (instruction : tree) (address : int list) =
    let entity = at instruction address in
    let color =
      if ((at entity [1;0]) = (Leaf "0")) then
        if (at instruction [0;0] = Leaf "create") then
          (Leaf "white")
        else
          (Leaf "unknown")
      else
        (at entity [1;0;0]) in
    let color = string_of_leaf color in
    let shape = string_of_leaf (at entity [1;1;0]) in
    let temp_direction = at entity [1;2] in
    let adjacent = if temp_direction = Leaf("0") then [] else
    let adj_address = if (temp_direction = Leaf("that")) then 3 else 2 in
    let direction = if ((at entity [1;adj_address;0;0]) = (Leaf "the") || (at entity [1;adj_address;0;0]) = (Leaf "0"))
                    then (at entity [1;adj_address;0;1])
                    else (at entity [1;adj_address;0;0]) in
    let direction = string_of_leaf direction in
    let (c, s, al) = extract_info m entity [1;adj_address;1] in
    let adjacent_entity = find_ID m (color_of_string c) (shape_of_string s) (List.map adjacent_of_string al) in
    [(direction,adjacent_entity)] in
    (color,shape,adjacent)

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
    String.filter (fun x -> x <> '0') (string_of_list ( (List.map (fun l -> "( " ^ string_of_list l ^ ")") (List.map (fun t -> (get_all_leaves []) t) tree_list))))

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

let create_command (m : model) (instruction : string) =
    let tree_list = (wrapper command instruction) in
    if List.length tree_list <> 0 then
      let tree = resolve_ambiguity tree_list in
      let (c, s, al) = extract_info m tree [0;1] in
      Create (Entity (!(genid ()), (shape_of_string s), (color_of_string c)), (List.map adjacent_of_string al))
    else
      raise Tree_not_found

let delete_command (m : model) (instruction : string) =
    let tree_list = (wrapper command instruction) in
    if List.length tree_list <> 0 then
      let tree = resolve_ambiguity tree_list in
      let (c, s, al) = extract_info m tree [0;1] in
      let id = find_ID m (color_of_string c) (shape_of_string s) (List.map adjacent_of_string al) in
      Delete (id)
    else
      raise Tree_not_found

let paint_command (m : model) (instruction : string) =
    let tree_list = (wrapper command instruction) in
    if List.length tree_list <> 0 then
      let tree = resolve_ambiguity tree_list in
      let (c, s, al) = extract_info m tree [0;1] in
      let id = find_ID m (color_of_string c) (shape_of_string s) (List.map adjacent_of_string al) in
      let new_color = string_of_leaf (at tree [0;2;0]) in
      Paint (id, color_of_string new_color)
    else
      raise Tree_not_found

let move_command (m : model) instruction =
    let tree_list = (wrapper command instruction) in
    if List.length tree_list <> 0 then
      let tree = resolve_ambiguity tree_list in
      let (c, s, al) = extract_info m tree [0;1] in
      let id = find_ID m (color_of_string c) (shape_of_string s) (List.map adjacent_of_string al) in
      let (c2, s2, al2) = extract_info m tree [0;3] in
      let id2 = find_ID m (color_of_string c2) (shape_of_string s2) (List.map adjacent_of_string al2) in
      let direction =
        if ((at tree [0;2;0]) = (Leaf "the") || (at tree [0;2;0]) = (Leaf "0"))
        then (at tree [0;2;1])
        else (at tree [0;2;0]) in
      let direction = string_of_leaf direction in
      Move (id, [Adjacent ((direction_of_string direction), id2)])
    else
      raise Tree_not_found

let parse (m : model) (instruction : string) =
  if instruction = "" then Error ("type something") else
    let first_word = List.hd (String.split_on_char ' ' instruction) in
    try
      match first_word with
      | "create" -> create_command m instruction
      | "delete" -> delete_command m instruction
      | "paint" -> paint_command m instruction
      | "move" -> move_command m instruction
      | "#exists" -> Exist (Exactly (3), (Red, Cube, []))
      | "#move" -> Move (3, [Adjacent (Right, 1)])
      | "#print" -> Print
      | "#reset" -> Reset
      | "#parse" -> List.iteri (writetree) (wrapper command (snd (String.split instruction " "))) ; Error ("parsed tree")
      | _ -> Error ("Unable to grasp meaning")
    with
    | No_such_adjacent_exception -> Error ("No such adjacent")
    | No_such_direction_exception -> Error ("No such direction")
    | No_such_shape_exception -> Error ("No such shape")
    | No_such_color_exception -> Error ("No such color")
    | No_such_entity_exception (msg) -> Error (Printf.sprintf "Unable to find this '%s' in the model" msg)
    | Tree_not_found -> Error ("Failed to parse input")
    (* | _ -> Error ("Something unexpected went wrong") *)
