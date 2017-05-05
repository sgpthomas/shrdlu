open Util

(* Types *)
type direction = Left | Right | Behind | Front | Above | Below
and shape = Cube | Sphere | Pyramid | Cylinder | Object
and color = Red | Orange | Yellow | Green | Blue | Purple | Unknown | Random
and adjacent = Adjacent of (direction * int)
and entity = Entity of int * shape * color
and model = (entity list) * (int * (adjacent list)) list

type determiner =
  | A
  | The
  | All
  | Number of int

(* Used for existence queries *)
type quantifier =
  | Less of int
  | Least of int
  | Exactly of int
  | Most of int
  | More of int


type command =
  | Create of int * color * shape * adjacent list * string
  | Delete of int list * string
  | Move of int list * adjacent list * string
  | Paint of int list * color * string
  | Find of color * shape * adjacent list * determiner
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

let verbose_direction = function
  | "left" -> "to the left of"
  | "right" -> "to the right of"
  | "behind" -> "behind"
  | "front" -> "in front of"
  | "above" -> "above"
  | "below" -> "below"
  | _ -> raise No_such_direction_exception

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
  | Cylinder -> "cylinder"
  | Object -> "object"

let shape_of_string = function
  | "cube" -> Cube
  | "sphere" -> Sphere
  | "pyramid" -> Pyramid
  | "cylinder" -> Cylinder
  | "object" -> Object
  | _ -> raise No_such_shape_exception

let string_of_color = function
  | Red -> "red"
  | Blue -> "blue"
  | Purple -> "purple"
  | Green -> "green"
  | Yellow -> "yellow"
  | Orange -> "orange"
  | Unknown -> ""
  | Random -> "random"

let color_of_string = function
  | "red" -> Red
  | "blue" -> Blue
  | "purple" -> Purple
  | "green" -> Green
  | "yellow" -> Yellow
  | "orange" -> Orange
  | "unknown" -> Unknown
  | "random" -> Random
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

let string_of_determiner = function
  | A -> "a"
  | The -> "the"
  | All -> "all"
  | Number (x) -> string_of_int (x)

let determiner_of_string (s : string) =
  match s with
  | "a" -> A
  | "the" -> The
  | "all" -> All
  | x -> Number (int_of_string x)

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
  | Unknown -> AnsiBoldGrey
  | Random -> AnsiWhite

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
    | [] -> raise (No_such_entity_exception (Printf.sprintf "debug3id: %d" id))
    | Entity (i, s, c)::tl -> if i = id then Entity (i, s, c) else f tl
  in
  let (entity_list, _) = m in f entity_list

let get_matches (m : model) (color : color) (shape : shape) (adj_list : adjacent list) =
  (* let rec print_list = function
    [] -> ()
    | e::l -> print_string(string_of_adjacent e) ; print_string " " ; print_list l in
  let () = print_list adj_list in *)
  let rec match_entity (el : entity list) (result : int list) =
    match el with
    | [] -> result
    | Entity (id, s, c)::tl ->
      match_entity tl (
        match color, shape with
        | Unknown, Object -> List.append result [id]
        | Unknown, sh -> if sh = s then (List.append result [id]) else result
        | col, Object -> if col = c then (List.append result [id]) else result
        | col, sh -> if col = c && sh = s then (List.append result [id]) else result
      )
  in

  let rec match_adjacent (num : (int * (adjacent list)) list) (i : int) =
    match num with
    | [] -> []
    | (n, al)::tl ->
      if n = i && List.for_all (fun x -> List.mem x al) (List.map flip_adjacent adj_list) then
        [n]
      else
        match_adjacent tl i
  in
  let (entity_list, adjacent_list) = m in
  List.flatten (List.map (match_adjacent (adjacent_list)) (match_entity entity_list []))


(* Helper function for retreiving the first n elements of a list *)
let rec first_n_elements n mylist =
  if n > List.length mylist then raise Not_enough_elements else
  match n with
  | 0 -> []
  | _ -> List.hd mylist :: first_n_elements (n-1) (List.tl mylist)


(*  *)
let return_ID_list (m : model) (c : color) (s : shape) (adj_list : adjacent list) (det : determiner) =
  let matches = get_matches m c s adj_list in
  let c = string_of_color c in
  let s = string_of_shape s in
  let ent = if c = "" then s else Printf.sprintf "%s %s" c s in
  match matches with
  | [] -> raise (No_such_entity_exception (Printf.sprintf "debug1%s" ent))
  | x ->
    let len = List.length matches in
    let () = (Printf.printf "Debug returnID: %d such %ss \n" len ent) in
    try
      match det with
      | A -> [List.hd matches]
      | The -> let () = if len > 1 then (Printf.printf "%d such %ss found. Choosing the first one\n" len ent) else () in
        [List.hd matches]
      | All -> matches
      | Number (n) ->
        if n > len then raise Not_enough_elements
        else first_n_elements n matches
    with
    | Not_enough_elements -> []

(* let model_of_ID_list (m : model) (id_list : int list) = *)
(*   let rec get_entities (el : entity list) = *)
(*     match el with *)
(*     | [] -> [] *)
(*     | hd :: tl -> if (List.exist (fun x -> )) *)

(*   let (entity_list, numbered_adj_list) = m in *)



let find_ID (m : model) (c : color) (s : shape) (adj_list : adjacent list) =
  let matches = get_matches m c s adj_list in

  match matches with
  | [] -> raise (No_such_entity_exception (Printf.sprintf "debug2%s %s" (string_of_color c) (string_of_shape s)))
  | x -> List.hd x


let find (m : model) (c : color) (s : shape) (adj_list : adjacent list) (det : determiner) =
  Response (string_of_int (find_ID m c s adj_list ), m)


let create_one (m : model) (e : entity) (adj_list : adjacent list) =
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
  let Entity (id, s, c) = e in
  let s = if s = Object then (shape_of_string (random_shape ())) else s in
  let c = if c = Random then (color_of_string (random_color ())) else c in
  let ent = Entity (id, s, c) in
  let (el, al) = m in (* get entities and adjacents *)
  let new_el = List.append el [ent] in
  let new_al = update_adjacents al id adj_list in
  let new_model = (new_el, new_al) in
  new_model

let rec create (m : model) (howmany : int) (c : color) (s : shape) (adj_list : adjacent list) (message : string) =
  match howmany with
    | 0 -> Response (message, m)
    | _ ->
      let e = Entity (!(genid ()), s, c) in
      let new_model = (create_one m e adj_list) in create new_model (howmany-1) c s adj_list message


let delete_one (m : model) (id : int) =
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
  new_model

let rec delete (m : model) (id_list : int list) (message : string) =
  match id_list with
  | [] -> Response (message, m)
  | head :: tail -> let new_model = (delete_one m head) in delete new_model tail message


let paint_one (m : model) (id : int) (nc : color) (message : string) =
  let paint_entity (el : entity list) =
    let re (e : entity) = let Entity (i, s, c) = e in if id = i then [Entity (i, s, nc)] else [e] in
    List.flatten (List.map re el)
  in

  let (el, al) = m in
  let new_model = (paint_entity el, al) in
  new_model

let rec paint (m : model) (id_list : int list) (nc : color) (message : string) =
  match id_list with
    | [] -> Response (message, m)
    | head :: tail -> let new_model = (paint_one m head nc message) in paint new_model tail nc message

let move_one (m : model) (id : int) (adj_list : adjacent list) =
  let rec check_adjacents (al : adjacent list) =
    match al with
    | [] -> true
    | hd :: tl ->
      let Adjacent (_, eid) = hd in if id = eid then false else true
  in

  let nm = delete_one m id in
  let e = (entity_of_id m id) in
  if check_adjacents adj_list then create_one nm e adj_list else raise Cant_self_reference

let rec move (m : model) (id_list : int list) (adj_list : adjacent list) (message : string) =
  try
    match id_list with
    | [] -> Response (message, m)
    | head :: tail -> let new_model = (move_one m head adj_list) in move new_model tail adj_list message
  with
  | Cant_self_reference -> Response ("can't move an object in reference to itself\n", m)
  | _ -> Response ("something unexpected went wrong while moving\n", m)


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
  | Create (howmany, c, s, adj_list, message) -> create m howmany c s adj_list message
  | Delete (id_list, message) -> delete m id_list message
  | Move (id_list, adj_list, message) -> move m id_list adj_list message
  | Paint (id_list, new_color, message) -> paint m id_list new_color message
  | Find (color, shape, adj_list, det) -> find m color shape adj_list det
  | Exist (quant, (c, s, al)) -> exists m quant c s al
  | Print -> print_model m ; Response ("", m)
  | Reset -> Response ("reset model\n", ([], []))
  | Error (msg) -> print_string msg ; print_newline () ; Response ("", m)

