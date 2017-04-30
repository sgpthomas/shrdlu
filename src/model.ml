(* Types *)
type direction = Left | Right | Behind | Front | Above | Below
and shape = Cube | Sphere | Pyramid
and color = Red | Blue | Purple | Green | Yellow | Orange
and adjacent = Adjacent of (direction * int)
and entity = Entity of int * shape * color
and model = (entity list) * (int * (adjacent list)) list

type command =
  | Create of entity * adjacent list
  | Delete of entity * adjacent list
  | Error of string
  | Print

type response = Response of string * model

exception No_such_adjacent_exception

(* Printing Functions *)
let string_of_direction = function
  | Left -> "left"
  | Right -> "right"
  | Behind -> "behind"
  | Front -> "front"
  | Above -> "above"
  | Below -> "below"

let string_of_shape = function
  | Cube -> "cube"
  | Sphere -> "sphere"
  | Pyramid -> "pyramid"

let string_of_color = function
  | Red -> "red"
  | Blue -> "blue"
  | Purple -> "purple"
  | Green -> "green"
  | Yellow -> "yellow"
  | Orange -> "orange"

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
  Printf.printf " - %i -> %s %s " id (string_of_color color) (string_of_shape shape) ;
  print_string "(" ; print_adjacent_list (get_adjacent_list m id) ; print_string ")" ; print_newline ()

let print_model (m : model) =
  let rec f l =
    match l with
    | [] -> ()
    | e::tl -> (print_entity m e) ; f tl
  in
  let (el, _) = m in
  f el

(* Model Functions *)
let update_adjacents (numbered : (int * (adjacent list)) list) (id : int) (adj_list : adjacent list) =
  let flip_adjacent (a : adjacent) = let (dir, id) = a in Adjacent (opposite_direction dir, id)

  List.append numbered (id, flip_list adj_list)


let create (m : model) (e : entity) (adj_list : adjacent list) =
  let (el, al) = m in (* get entities and adjacents *)
  let new_el = List.append el [e] in
  let new_al = update_adjacents al adj_list in
  let new_model = (new_el, new_al) in
  let Entity (id, shape, color) = e in
  let msg = Printf.sprintf "created %d: %s %s" id (string_of_color color) (string_of_shape shape) in
  Response (msg, new_model)

let delete (m : model) (e : entity) (al : adjacent list) =
  Response ("delete", m)

let perform (c : command) (m : model) =
  match c with
  | Create (ent, adj_list) -> create m ent adj_list
  | Delete (ent, adj_list) -> delete m ent adj_list
  | Print -> print_model m ; Response ("", m)
  | Error (msg) -> print_string msg ; print_newline () ; Response ("", m)


