open Util
open Model
open Grammar
open Batteries

(* Returns the string of a leaf *)
let string_of_leaf = function
  | Leaf(str) -> str
  | _ -> raise Not_a_leaf


(* Finds the determiner at a given address. "a" if none *)
let find_det tree address =
  if (at tree address) = (Leaf "0") then "a" else (* Set null determiner equivalent to "a" *)
  let address = (List.append address [0]) in 
  let num_or_det = at tree address in
    match num_or_det with
    | Branch("Number",_) -> string_of_leaf (at num_or_det [0])
    | _ -> string_of_leaf num_or_det

(* Ensures that proper determiners are being used *)
let check_det command_type det =
  match  command_type with
  | "adjacent" -> if (det = "the" || det = "a") then true
                  else false
  | "create" -> if (det = "the" || det = "all") then false
                else true
  | "delete" -> true
  | "paint"  -> true
  | "move"   -> if (det = "a" || det = "the") then true
                else false
  | _ -> raise No_such_command_exception


(* The most important function in parser. Given the address of an entity
 * a tree, it will extract its color, shape, and adjacent entities. Works
 * recursively by also extracting the info of the adjacent entities.
 *)
let extract_info (m : model) (instruction : tree) (address : int list) =
  let resolve (check : int list) (adj_list : adjacent list) =
    let rec f al i =
      match al with
      | [] -> []
      | hd::tl ->
        (* if in the model nal, [id] has (dir: check) then return this and recur on rest of list
           else just recur on rest of list*)
        let Adjacent (dir, id) = hd in
        let l = get_adjacent_list m id in
        if List.mem (Adjacent (dir, i)) l then hd::(f tl i) else f tl i
    in
    List.flatten (List.map (f adj_list) check)
  in
  let rec extract (instruction : tree) (address : int list) =
    let entity = at instruction address in
    let color = string_of_leaf (
        if ((at entity [1;0]) = (Leaf "0")) then
          match (at instruction [0]) with
          (* If color not defined for create command, default to white *)
          | Branch ("Create", _) -> (Leaf ("random"))
          (* If color not defined for other commands, then color unknown *)
          | _ -> (Leaf "unknown")
          (* Color has been explicitly defined *)
        else (at entity [1;0;0])
      ) in
    let shape = string_of_leaf (at entity [1;1;0]) in
    let maybe_direction = at entity [1;2] in
    let adjacent =
      (* No adjacent entities *)
      if (maybe_direction = Leaf("0")) then
        []
      else
        let adj_address = if (maybe_direction = Leaf("that")) then 3 else 2 in
        let det = find_det entity [1;adj_address;1;0] in
        if not (check_det "adjacent" det) then raise Incorrect_determiner else
          let direction =
            string_of_leaf (
              if ((at entity [1;adj_address;0;0]) = (Leaf "the") || (at entity [1;adj_address;0;0]) = (Leaf "0"))
              then (at entity [1;adj_address;0;1])
              else (at entity [1;adj_address;0;0])
            ) in
          let (c, s, al) = extract entity [1;adj_address;1] in
          (* let adjacent_entity = find_ID m c s al in *)
          let possible_IDs = return_ID_list m c s al (determiner_of_string det) in
          List.map (fun x -> (direction, x)) possible_IDs
    in
    let c, s = (color_of_string color, shape_of_string shape) in
    if List.length adjacent > 1 then
      (c, s, resolve (get_matches m c s []) (List.map adjacent_of_string adjacent))
    else
      (c, s, List.map adjacent_of_string adjacent)
  in
  extract instruction address

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
  let det = find_det tree [0;1;0] in
  let howmany = if det = "a" then 1 else int_of_string det in
  if not (check_det "create" det) then raise Incorrect_determiner else
  let (c, s, al) = extract_info m tree [0;1] in
  let message = Printf.sprintf 
    "created %d %s %s" howmany (string_of_color c) (string_of_shape s) in
  let message = if howmany > 1 then message^"s\n" else message^"\n" in
  Create (howmany, c, s, al, message)


(* Returns a delete command given a string and model *)
let delete_command (m : model) (instruction : string) =
  let tree = get_tree instruction in
  let det = find_det tree [0;1;0] in
  if not (check_det "delete" det) then raise Incorrect_determiner else
    let (c, s, al) = extract_info m tree [0;1] in
    let id_list = return_ID_list m c s al (determiner_of_string det) in
    let len = List.length id_list in
    let message = Printf.sprintf 
      "deleted %d %s %s" len (string_of_color c) (string_of_shape s) in
    let message = if len > 1 then message^"s\n" else message^"\n" in
    Delete (id_list, message)


(* Returns a paint command given a string and model *)
let paint_command (m : model) (instruction : string) =
  let tree = get_tree instruction in
  let det = find_det tree [0;1;0] in
  if not (check_det "paint" det) then raise Incorrect_determiner else
    let (c, s, al) = extract_info m tree [0;1] in
    let id_list = return_ID_list m c s al (determiner_of_string det) in
    let new_color = string_of_leaf (at tree [0;2;0]) in
    let len = List.length id_list in
    let message = Printf.sprintf
      "painted %d %s %s %s" len (string_of_color c) (string_of_shape s) new_color in
    let message = if len > 1 then message^"s\n" else message^"\n" in
    Paint (id_list, color_of_string new_color, message)


(* Returns a move command given a string and model *)
let move_command (m : model) (instruction : string) =
  let tree = get_tree instruction in
  let det1 = find_det tree [0;1;0] in (* Determiner of the first entity *)
  let det2 = find_det tree [0;3;0] in (* Determiner of the second entity *)
  if not (check_det "move" det2) then raise Incorrect_determiner else
    let (c, s, al) = extract_info m tree [0;1] in
    let id_list = return_ID_list m c s al (determiner_of_string det1) in
    let (c2, s2, al2) = extract_info m tree [0;3] in
    let id2 = find_ID m c2 s2 al2 in
    let direction = string_of_leaf (
      if ((at tree [0;2;0]) = (Leaf "the") || (at tree [0;2;0]) = (Leaf "0"))
      then (at tree [0;2;1])
      else (at tree [0;2;0]) 
    ) in
    let len = List.length id_list in
    let message = Printf.sprintf "moved %d %s %s" len (string_of_color c) 
      (string_of_shape s) in
    let message = if len > 1 then message^"s" else message in
    let message = message^(Printf.sprintf " %s the %s %s\n" (verbose_direction direction) (string_of_color c2) (string_of_shape s2)) in
    Move (id_list, [Adjacent (direction_of_string direction, id2)], message)


(* Returns an exists command given a string and model *)
let exists_command (m : model) (instruction : string) =
  let tree = get_tree instruction in
    let quantifier = quantifier_of_string (string_of_leaf (at tree [0;0;1;0]))
    (int_of_string (string_of_leaf (at tree [0;0;1;1;0]))) in
    let (c, s, al) = extract_info m tree [0;0;2] in
    Exist (quantifier, (c, s,al))


(* Returns a howmany command given a string and a model *)
let howmany_command (m : model) (instruction : string) =
  let tree = get_tree instruction in
    let (c, s, al) = extract_info m tree [0;0;2] in
    Howmany (c, s, al)


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
    | "how" -> howmany_command m instruction
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
  (* | No_such_entity_exception (msg) -> Error (Printf.sprintf "Unable to find this '%s' in the model" msg) *)
  | Tree_not_found -> Error ("Failed to parse input")
  | Not_a_leaf -> Error ("Called string_of_leaf incorrectly")
  | Incorrect_determiner -> Error ("Used an incorrect determiner")
  (* | _ -> Error ("Something unexpected went wrong") *)
