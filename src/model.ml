(* Types *)
type direction = Left | Right | Behind | Front | Above | Below
and shape = Cube | Sphere | Pyramid
and color = Red | Blue | Purple | Green | Yellow | Orange | Colorless
and adjacent = Adjacent of (direction * int)
and entity = Entity of int * shape * color
and model = (entity list) * (int * (adjacent list)) list

type command =
  | Create of entity * adjacent list
  | Delete of int
  | Find of color * shape * adjacent list
  | Paint of int * color
  | Print
  | Reset
  | Error of string

type response = Response of string * model

exception No_such_adjacent_exception
exception No_such_direction_exception
exception No_such_shape_exception
exception No_such_color_exception
exception No_such_entity_exception

(* Printing Functions *)
let string_of_direction = function
  | Left -> "left"
  | Right -> "right"
  | Behind -> "behind"
  | Front -> "front"
  | Above -> "above"
  | Below -> "below"

let direction_of_string = function
  | "left" -> Left
  | "right" -> Right
  | "behind" -> Behind
  | "front" -> Front
  | "above" -> Above
  | "below" -> Below
  | _ -> raise No_such_direction_exception

let adjacent_of_string = function
  | (dir, id) -> Adjacent (direction_of_string dir, id)

let string_of_shape = function
  | Cube -> "cube"
  | Sphere -> "sphere"
  | Pyramid -> "pyramid"

let shape_of_string = function
  | "cube" -> Cube
  | "sphere" -> Sphere
  | "pyramid" -> Pyramid
  | _ -> raise No_such_shape_exception

let string_of_color = function
  | Red -> "red"
  | Blue -> "blue"
  | Purple -> "purple"
  | Green -> "green"
  | Yellow -> "yellow"
  | Orange -> "orange"
  | Colorless -> "colorless"

let color_of_string = function
  | "red" -> Red
  | "blue" -> Blue
  | "purple" -> Purple
  | "green" -> Green
  | "yellow" -> Yellow
  | "orange" -> Orange
  | "colorless" -> Colorless
  | _ -> raise No_such_color_exception

let opposite_direction = function
  | Left -> Right
  | Right -> Left
  | Behind -> Front
  | Front -> Behind
  | Above -> Below
  | Below -> Above

let genid =
  let n = ref 0 in
  function () -> n := (!n) + 1 ; n

let get_adjacent_list (m : model) (id : int) =
  let rec find_adjacent l =
    match l with
    | [] -> []
    | (i, al)::tl -> if i = id then al else find_adjacent tl
  in
  let (_, al) = m in
  find_adjacent al

let print_entity (m : model) (e : entity) =
  let print_adjacent a =
    let Adjacent (dir, id) = a in
    Printf.printf " %s: %i " (string_of_direction dir) id
  in
  let rec print_adjacent_list al =
    match al with
    | [] -> ()
    | a::tl -> print_adjacent a ; print_adjacent_list tl
  in

  let Entity (id, shape, color) = e in
  Printf.printf " [%i] -> %s %s " id (string_of_color color) (string_of_shape shape) ;
  print_string "(" ; print_adjacent_list (get_adjacent_list m id) ; print_string ")" ; print_newline ()

let print_model (m : model) =
  let rec f l =
    match l with
    | [] -> ()
    | e::tl -> (print_entity m e) ; f tl
  in
  let (el, _) = m in
  f el

(****** Model Functions ******)

(* Reverse the direction of an adjacent *)
let flip_adjacent (a : adjacent) =
  let Adjacent (dir, i) = a in Adjacent (opposite_direction dir, i)

let find_ID (m : model) (color : color) (shape : shape) (adj_list : adjacent list) =
  let print_adjacent a =
    let Adjacent (dir, id) = a in
    Printf.printf " %s: %i " (string_of_direction dir) id
  in
  let rec print_adjacent_list al =
    match al with
    | [] -> ()
    | a::tl -> print_adjacent a ; print_adjacent_list tl
  in
  let () = Printf.printf "%s %s" (string_of_color color) (string_of_shape shape) in let () = print_adjacent_list adj_list ; print_newline () in
  let rec match_entity (el : entity list) =
    match el with
    | [] -> raise No_such_entity_exception
    | Entity (id, s, c)::tl -> if s = shape && c = color then id else match_entity tl
  in

  let rec match_adjacent (num : (int * (adjacent list)) list) id =
    match num with
    | [] -> raise No_such_entity_exception
    | (n, al)::tl ->
      if n = id && List.for_all (fun x -> List.mem x al) (List.map flip_adjacent adj_list) then
        n
      else
        match_adjacent tl id
  in

  let (entity_list, adjacent_list) = m in
  match_adjacent (adjacent_list) (match_entity entity_list)

let find (m : model) (c : color) (s : shape) (adj_list : adjacent list) =
  Response (string_of_int (find_ID m c s adj_list), m)

let create (m : model) (e : entity) (adj_list : adjacent list) =
  (* Given a numbered list, an entity id, and an adjacent list, update the numbered list *)
  let update_adjacents (numbered : (int * (adjacent list)) list) (id : int) (adj_list : adjacent list) =


    (* If the id of the adjacent matches the id of the numbered row, add adjacent to that row *)
    let update_number (a : adjacent) (nal : (int * adjacent list)) =
      let Adjacent (dir, i) = a in
      let (n, al) = nal in
      if i = n then
        (n, List.append al [Adjacent (dir, id)])
      else
        nal
    in

    (* Apply update number to every row in the numbered list *)
    let update (num : (int * (adjacent list)) list) (a : adjacent) =
      List.map (update_number a) num
    in

    (* Add opposite of given adjacents to a new row in the numbered list *)
    let new_numbered = List.append numbered [(id, (List.map flip_adjacent adj_list))] in

    (* If no adjacencies, do nothing special, otherwise apply  *)
    if List.length adj_list <> 0 then
      List.hd (List.rev (List.map (update new_numbered) adj_list))
    else
      new_numbered
  in

  let Entity (id, shape, color) = e in
  let (el, al) = m in (* get entities and adjacents *)
  let new_el = List.append el [e] in
  let new_al = update_adjacents al id adj_list in
  let new_model = (new_el, new_al) in
  let msg = Printf.sprintf "created %d: %s %s" id (string_of_color color) (string_of_shape shape) in
  Response (msg, new_model)

let delete (m : model) (id : int) =
  (* Given a numbered list, an entity id, and an adjacent list, update the numbered list *)
  let update_adjacents (numbered : (int * (adjacent list)) list) (id : int) =

    let remove_row (num : int * adjacent list)  =
      let (i, _) = num in id <> i
    in

    let remove_links (num : int * adjacent list) =
      let (i, al) = num in
      let f a = let Adjacent (_, i) = a in id <> i in
      (i, List.filter f al)
    in

    List.map (remove_links) (List.filter remove_row numbered)
  in

  let remove_entity (el : entity list) =
    let re (e : entity) = let Entity (i, _, _) = e in if id = i then [] else [e] in
    List.flatten (List.map re el)
  in

  let (el, al) = m in (* get entities and adjacents *)
  let new_el = remove_entity el in
  let new_al = update_adjacents al id in
  let new_model = (new_el, new_al) in
  let msg = Printf.sprintf "deleted %d" id in
  Response (msg, new_model)

let paint (m : model) (id : int) (nc : color) =
  let paint_entity (el : entity list) =
    let re (e : entity) = let Entity (i, s, c) = e in if id = i then [Entity (i, s, nc)] else [e] in
    List.flatten (List.map re el)
  in

  let (el, al) = m in
  let new_model = (paint_entity el, al) in
  let msg = Printf.sprintf "painted %d %s" id (string_of_color nc) in
  Response (msg, new_model)

let perform (c : command) (m : model) =
  match c with
  | Create (ent, adj_list) -> create m ent adj_list
  | Delete (id) -> delete m id
  | Find (color, shape, adj_list) -> find m color shape adj_list
  | Paint (id, new_color) -> paint m id new_color
  | Print -> print_model m ; Response ("", m)
  | Reset -> Response ("reset model", ([], []))
  | Error (msg) -> print_string msg ; print_newline () ; Response ("", m)

