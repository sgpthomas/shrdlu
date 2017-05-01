open Model
open Grammar
open Batteries

let dot_of_tree title t =
  let rec dot_of_node i = function
      Leaf name ->  (("n"^(string_of_int i)^" [label = \""^name^"\"];\n"),i)
    | Branch (parent,kids) ->
        let (rootbyindexlist,maximum) = List.fold_left (fun (sofar,index) kid ->
          let (result,newindex) = dot_of_node (index+1) kid in
          ((result,(index+1))::sofar,newindex)
                                        )
            ([],i)
            kids in
        let thisnode = ("n"^(string_of_int i)^" [label = \""^parent^"\"];\n") in
        let downarrows = List.fold_left (fun already (subtree,index) ->
          ("n"^(string_of_int i)^"-> n"^(string_of_int index)^";\n"^already)
                        )
            ""
            rootbyindexlist in
        let subtreedot = List.fold_left (fun already (subtree,index) ->
                                             subtree^already)
            ""
            rootbyindexlist in
        (thisnode^downarrows^subtreedot,maximum)

 in
  ("digraph \""^title^"\" {\n node [shape = plaintext]; \n edge [arrowhead = none]; \n"^(Pervasives.fst (dot_of_node 0 t))^"}")

let writetree i t =
  let name = Printf.sprintf "tree%i" i in
  let oc = open_out name in
  output_string oc (dot_of_tree name t);
  close_out oc;
  Sys.command (Printf.sprintf "dot -Tpng %s -O" name);
  Sys.command (Printf.sprintf "rm %s" name);
  ()

let rec extract_info model instruction_tree address =
    let entity = at instruction_tree address in
    let color = if ((at entity [0;1]) = (Leaf "0")) then (Leaf "colorless")
                else (at entity [0;1;0]) in
    let Leaf(color) = color in
    let Leaf(shape) = at entity [0;2;0] in
    let temp_direction = at entity [0;3] in
    let adjacent = if temp_direction = Leaf("0") then [] else
    let adj_address = if (temp_direction = Leaf("that")) then 4 else 3 in
    let direction = if ((at entity [0;adj_address;0;0]) = (Leaf "the") || (at entity [0;adj_address;0;0]) = (Leaf "0"))
                    then (at entity [0;adj_address;0;1])
                    else (at entity [0;adj_address;0;0]) in
    let Leaf(direction) = direction in
    let (c, s, al) = extract_info model entity [0;adj_address;1] in
    let adjacent_entity = find_ID model (color_of_string c) (shape_of_string s) (List.map adjacent_of_string al) in
    [(direction,adjacent_entity)] in
    (color,shape,adjacent);;

let create_command model instruction =
    let tree_list = (wrapper command instruction) in
    if List.length tree_list <> 0 then
      let tree1 = List.hd tree_list in
      let (c, s, al) = extract_info model tree1 [0;1] in
      Create (Entity (!(genid ()), (shape_of_string s), (color_of_string c)), (List.map adjacent_of_string al))
    else
      raise Tree_not_found

let delete_command model instruction =
    let tree_list = (wrapper command instruction) in
    if List.length tree_list <> 0 then
      let tree1 = List.hd tree_list in
      let (c, s, al) = extract_info model tree1 [0;1] in
      let id = find_ID model (color_of_string c) (shape_of_string s) (List.map adjacent_of_string al) in
      Delete (id)
    else
      raise Tree_not_found

let paint_command model instruction =
    let tree_list = (wrapper command instruction) in
    if List.length tree_list <> 0 then
      let tree1 = List.hd tree_list in
      let (c, s, al) = extract_info model tree1 [0;1] in
      let id = find_ID model (color_of_string c) (shape_of_string s) (List.map adjacent_of_string al) in
      let Leaf(new_color) = at tree1 [0;2;0] in
      Paint (id, color_of_string new_color)
    else
      raise Tree_not_found

(* let move_command mode instruction =
    let tree_list = (wrapper command instruction) in
    if List.length tree_list <> 0 then
      let tree1 = List.hd tree_list in
      let (c, s, al) = extract_info model tree1 [0;1] in
      let id = find_ID model (color_of_string c) (shape_of_string s) (List.map adjacent_of_string al) in
      let new_color = at tree1 [0;2;1;0] in
      Paint (id, color)
    else
      raise Tree_not_found *)

let parse model instruction =
    let first_word = List.hd (String.split_on_char ' ' instruction) in
    try
      match first_word with
      | "create" -> create_command model instruction
      | "delete" -> delete_command model instruction
      | "paint" -> paint_command model instruction
      (* | "move" -> move_command model instruction *)
      | "#print" -> Print
      | "#reset" -> Reset
      | _ -> Error ("Unable to grasp meaning")
    with
    | No_such_adjacent_exception -> Error ("No such adjacent")
    | No_such_direction_exception -> Error ("No such direction")
    | No_such_shape_exception -> Error ("No such shape")
    | No_such_color_exception -> Error ("No such color")
    | No_such_entity_exception -> Error ("No such entity")
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
