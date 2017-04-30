open Model
open Grammar

let parse instruction = 
    let first_word = fst (String.split instruction " ") in
    if first_word = "create" then create instruction else
    "output to user that they messed up";;



let rec extract_info instruction_tree address =
    let entity = at instruction_tree address in 
    let color = if ((at entity [0;1]) = (Leaf "0")) then (Leaf "colorless") 
                else (at entity [0;1;0]) in
    let Leaf(color) = color in 
    let Leaf(shape) = at entity [0;2;0] in
    let temp_direction = at entity [0;3] in
    let adjacent = if temp_direction = Leaf("0") then [] else
    let direction = if ((at entity [0;3;0;0]) = (Leaf "the") || (at entity [0;3;0;0]) = (Leaf "0"))
                    then (at entity [0;3;0;1])
                    else (at entity [0;3;0;0]) in 
    let Leaf(direction) = direction in 
    let adjacent_entity = find_ID (extract_info entity [0;3;1]) in
    [(direction,adjacent_entity)] in 
    (color,shape,adjacent);;

let create instruction = 
    let tree_list = (wrapper command instruction) in
    let tree1 = List.hd tree_list in
    let color = if ((at tree1 [0;1;0;1]) = (Leaf "0")) then (Leaf "colorless") 
                else (at tree1 [0;1;0;1;0]) in 
    let Leaf(color) = color in 
    let Leaf(shape) = at tree1 [0;1;0;2;0] in
    let direction = if ((at tree1 [0;1;0;3;0;0]) = (Leaf "the") || (at tree1 [0;1;0;3;0;0]) = (Leaf "0"))
                    then (at tree1 [0;1;0;3;0;1])
                    else (at tree1 [0;1;0;3;0;0]) in 
    let Leaf(direction) = direction in 
    direction;;



let parse (s : string) =
  match s with
  | "create 1" -> Create (Entity (!(genid ()), Cube, Red), [])
  | "create 2" -> Create (Entity (!(genid ()), Sphere, Blue), [Adjacent (Right, 1)])
  | "print" -> Print
  | _ -> Error ("Cannot grasp meaning.")

["create";"red";"cube";""]
