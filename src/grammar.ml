(* Tree type definitions *)
type tree = Leaf of string | Branch of string * tree list
let unary label child = Branch (label,[child])
let binary label (child1,child2) = Branch (label,[child1;child2])
let ternary label ((child1,child2),child3) = Branch (label,[child1;child2;child3])
let fourary label (((child1,child2),child3),child4) = Branch (label,[child1;child2;child3;child4])
let fiveary label ((((child1,child2),child3),child4),child5) = Branch (label,[child1;child2;child3;child4;child5])

type remainder = string list
type 'a combinatorparser = remainder -> ('a * remainder) list

let (terminal : string -> tree combinatorparser) myword = function
  | x::xs when x=myword -> [((Leaf myword),xs)]
  | _ -> ([] : (tree * remainder) list)

let ( |. ) (p : 'a combinatorparser) (q : 'a combinatorparser) xs = List.append (p xs) (q xs)

let ( &. ) (p : 'a combinatorparser) (q : 'b combinatorparser) xs =
  [? List:((r1,r2),zs) | (r1,ys) <- List:(p xs); (r2,zs) <- List:(q ys) ?]

let ( >. ) (p : 'a combinatorparser) f xs =
  [? List:(f x,ys) | (x,ys) <- List:(p xs) ?]

(* Define empty and optional *)
let (empty : tree combinatorparser) words = [((Leaf "0"), words)]
let opt p = p |. empty

(* The custom grammar itself *)
let rec command words = (create |. delete |. paint |. move >. unary "Command") words
and create words = ((terminal "create") &. entity >. binary "Create") words
and delete words = ((terminal "delete") &. entity >. binary "Delete") words
and paint words = ((terminal "paint") &. entity &. color >. ternary "Paint") words
and move words = ((terminal "move") &. entity &. direction &. entity >. fourary "Move") words
and entity words = (thatLessentity |. thatFulentity >. unary "Entity") words
and thatLessentity words = ( (opt(d) &. opt(color) &. shape &. opt(adjacent)) >. fourary "thatLessEntity") words
and thatFulentity words = ( (opt(d) &. opt(color) &. shape &. (terminal "that") &. adjacent) >. fiveary "thatFulEntity") words
and adjacent words = (direction &. entity >. binary "Adjacent") words
and d = (terminal "the" |. terminal "a" |. terminal "an") >. unary "D"
and shape = (terminal "cube" |. terminal "sphere" |. terminal "pyramid") >. unary "Shape"
and color = (terminal "red" |. terminal "orange" |. terminal "yellow" |. terminal "white" |. terminal "green" |. terminal "blue" |. terminal "purple") >. unary "Color"
and direction = ((terminal "above" |. terminal "below" |. terminal "behind" |. terminal "front") >. unary "Direction")
      |. ((opt(terminal "the") &. (terminal "left" |. terminal "right")) >. binary "Direction")

(* Wrapper that parses the sentence *)
let wrapper (p : tree combinatorparser) words =
  let remove_garbage words =
    let predicate x = (x <> "to") && (x <> "in") && (x <> "of") && (x <> "is") in List.filter predicate words
  in
  let finished analysis = match analysis with
    | (_,[]) -> true
    | _ -> false
  in
  List.map Pervasives.fst (List.filter finished
                             (p (remove_garbage (String.split_on_char ' ' words))))

(* Address finder *)
exception Tree_not_found
let rec at myTree address =
  match address with
  | [] -> myTree
  | head :: tail -> match myTree with
    | Leaf(_) -> if [head] = [] then myTree else raise Tree_not_found
    | Branch(value, tree_list) -> if head >= List.length tree_list then raise Tree_not_found else
        at (List.nth tree_list head) tail

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
  let _ = Sys.command (Printf.sprintf "dot -Tpng %s -O" name) in
  let _ = Sys.command (Printf.sprintf "rm %s" name) in
  ()

let produce_trees (sl : string list) =
  let tl = List.flatten (List.map (wrapper command) sl) in
  List.iteri writetree tl
