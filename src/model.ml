(* Types *)
type direction = Left | Right | Behind | Front | Above | Below
and shape = Cube | Sphere | Pyramid
and color = Red | Blue | Purple | Green | Yellow | Orange
and adjacent = Adjacent of (direction * entity)
and entity = Entity of shape * color * adjacent list
and model = entity list

type command =
  | Create of entity * adjacent list
  | Delete of entity * adjacent list
  | Error of string
  | Print

type response = Response of string * model

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

let print_entity (e : entity) =
  let print_adjacent a =
    let Adjacent (dir, ent) = a in
    let Entity (shape, color, _) = ent in
    Printf.printf "  %s: %s %s\n" (string_of_direction dir) (string_of_color color) (string_of_shape shape)
  in

  let Entity (s, c, (al : adjacent list)) = e in
  Printf.printf " - Shape: %s, Color: %s\n" (string_of_shape s) (string_of_color c) ;
  List.iter print_adjacent al

let print_model (m : model) =
  List.iter print_entity m

(* Model Functions *)
let create (m : model) (e : entity) (al : adjacent list) =
  let new_model = List.append m [e] in
  let Entity (shape, color, _) = e in
  let msg = Printf.sprintf "created new %s %s" (string_of_color color) (string_of_shape shape) in
  Response (msg, new_model)

let delete (m : model) (e : entity) (al : adjacent list) =
  Response ("delete", m)

let perform (c : command) (m : model) =
  match c with
  | Create (ent, adj_list) -> create m ent adj_list
  | Delete (ent, adj_list) -> delete m ent adj_list
  | Print -> print_model m ; Response ("", m)
  | Error (msg) -> print_string msg ; print_newline () ; Response ("", m)

