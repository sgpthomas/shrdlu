open Util

(* Exceptions *)
exception No_such_quantifier_exception
exception No_such_adjacent_exception
exception No_such_direction_exception
exception No_such_shape_exception
exception No_such_color_exception
exception No_such_entity_exception of string

(* Types *)
type direction = Left | Right | Behind | Front | Above | Below
and shape = Cube | Sphere | Pyramid | Object
and color = Red | Orange | Yellow | Green | Blue | Purple | White | Unknown
and adjacent = Adjacent of (direction * int)
and entity = Entity of int * shape * color
and model = (entity list) * (int * (adjacent list)) list

(* Used for existence queries *)
type quantifier =
  | Less of int
  | Least of int
  | Exactly of int
  | Most of int
  | More of int


type command =
  | Create of entity * adjacent list
  | Delete of int
  | Move of int * adjacent list
  | Paint of int * color
  | Find of color * shape * adjacent list
  | Exist of quantifier * (color * shape * adjacent list)
  | Print
  | Reset
  | Error of string

type response = Response of string * model

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

let string_of_shape = function
  | Cube -> "cube"
  | Sphere -> "sphere"
  | Pyramid -> "pyramid"
  | Object -> "object"

let shape_of_string = function
  | "cube" -> Cube
  | "sphere" -> Sphere
  | "pyramid" -> Pyramid
  | "object" -> Object
  | _ -> raise No_such_shape_exception

let string_of_color = function
  | Red -> "red"
  | Blue -> "blue"
  | Purple -> "purple"
  | Green -> "green"
  | Yellow -> "yellow"
  | Orange -> "orange"
  | White -> "white"
  | Unknown -> "unknown"

let color_of_string = function
  | "red" -> Red
  | "blue" -> Blue
  | "purple" -> Purple
  | "green" -> Green
  | "yellow" -> Yellow
  | "orange" -> Orange
  | "white" -> White
  | "unknown" -> Unknown
  | _ -> raise No_such_color_exception

let adjacent_of_string = function
  | (dir, id) -> Adjacent (direction_of_string dir, id)

let string_of_adjacent = function
  | Adjacent(dir, id) -> (string_of_direction dir) ^ " " ^ (string_of_int id)

let string_of_quantifier = function
  | Less (_) -> "less"
  | Least (_) -> "least"
  | Exactly (_) -> "exactly"
  | Most (_) -> "most"
  | More (_) -> "more"

let quantifier_of_string (s : string) (n : int) =
  match s with
  | "less" -> Less (n)
  | "least" -> Least (n)
  | "exactly" -> Exactly (n)
  | "most" -> Most (n)
  | "more" -> More (n)
  | _ -> raise No_such_quantifier_exception

let opposite_direction = function
  | Left -> Right
  | Right -> Left
  | Behind -> Front
  | Front -> Behind
  | Above -> Below
  | Below -> Above

(* Function used to create new id's for entities *)
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

let print_adjacent_list al =
  let pa a =
    let Adjacent (dir, id) = a in
    Printf.printf " %s: %i " (string_of_direction dir) id
  in
  let rec pal al =
    match al with
    | [] -> ()
    | a::tl -> pa a ; pal tl
  in
  pal al

let get_ansi_of_color = function
  | Red -> AnsiRed
  | Blue -> AnsiBlue
  | Purple -> AnsiPurple
  | Green -> AnsiGreen
  | Yellow -> AnsiYellow
  | Orange -> AnsiOrange
  | White -> AnsiWhite
  | Unknown -> AnsiBoldGrey

let print_entity (m : model) (e : entity) =
  let Entity (id, shape, color) = e in
  print_color (Printf.sprintf " [%i] -> %s %s " id (string_of_color color) (string_of_shape shape)) (get_ansi_of_color (color)) ;
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

let entity_of_id (m : model) (id : int) =
  let rec f (el : entity list) =
    match el with
    | [] -> raise (No_such_entity_exception (Printf.sprintf "id: %d" id))
    | Entity (i, s, c)::tl -> if i = id then Entity (i, s, c) else f tl
  in
  let (entity_list, _) = m in f entity_list

let get_matches (m : model) (color : color) (shape : shape) (adj_list : adjacent list) =
  let rec match_entity (el : entity list) (result : int list) =
    match el with
    | [] -> result
    | Entity (id, s, c)::tl ->
      match_entity tl (
        match color, shape with
        | Unknown, Object -> result
        | Unknown, sh -> if sh = s then (List.append result [id]) else result
        | col, Object -> if col = c then (List.append result [id]) else result
        | col, sh -> if col = c && sh = s then (List.append result [id]) else result
      )
  in

  let rec match_adjacent (num : (int * (adjacent list)) list) (i : int) =
    let color = match color with
      | Unknown -> ""
      | x -> string_of_color x
    in
    let shape = match shape with
      | Object -> ""
      | y -> string_of_shape y
    in
    match num with
    | [] -> raise (No_such_entity_exception (Printf.sprintf "%s %s" color shape))
    | (n, al)::tl ->
      if n = i && List.for_all (fun x -> List.mem x al) (List.map flip_adjacent adj_list) then
        n
      else
        match_adjacent tl i
  in
  let (entity_list, adjacent_list) = m in
  List.map (match_adjacent (adjacent_list)) (match_entity entity_list [])

let find_ID (m : model) (c : color) (s : shape) (adj_list : adjacent list) =
  let matches = get_matches m c s adj_list in
  match matches with
  | [] -> raise (No_such_entity_exception (Printf.sprintf "%s %s" (string_of_color c) (string_of_shape s)))
  | x -> List.hd x

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
  let msg = Printf.sprintf "created %d: %s %s\n" id (string_of_color color) (string_of_shape shape) in
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
  let msg = Printf.sprintf "deleted %d\n" id in
  Response (msg, new_model)

let move (m : model) (id : int) (adj_list : adjacent list) =
  let Response (_, nm) = delete m id in
  let Response (_, new_model) = create nm (entity_of_id m id) adj_list in
  Response (Printf.sprintf "moved %d\n" id, new_model)

let paint (m : model) (id : int) (nc : color) =
  let paint_entity (el : entity list) =
    let re (e : entity) = let Entity (i, s, c) = e in if id = i then [Entity (i, s, nc)] else [e] in
    List.flatten (List.map re el)
  in

  let (el, al) = m in
  let new_model = (paint_entity el, al) in
  let msg = Printf.sprintf "painted %d %s\n" id (string_of_color nc) in
  Response (msg, new_model)

let exists (m : model) (quant : quantifier) (color : color) (shape : shape) (adj_list : adjacent list) =
  let res = get_matches m color shape adj_list in
  let num_items = List.length res in 
  let b = match quant with
    | Less (n) -> num_items < n
    | Least (n) -> num_items >= n
    | Exactly (n) -> num_items = n
    | Most (n) -> num_items <= n
    | More (n) -> num_items > n
  in
  let msg = if b then "yes" else "no" in Response (msg^"\n", m)

let perform (c : command) (m : model) =
  match c with
  | Create (ent, adj_list) -> create m ent adj_list
  | Delete (id) -> delete m id
  | Move (id, adj_list) -> move m id adj_list
  | Paint (id, new_color) -> paint m id new_color
  | Find (color, shape, adj_list) -> find m color shape adj_list
  | Exist (quant, (c, s, al)) -> exists m quant c s al
  | Print -> print_model m ; Response ("", m)
  | Reset -> Response ("reset model\n", ([], []))
  | Error (msg) -> print_string msg ; print_newline () ; Response ("", m)

