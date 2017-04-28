type direction = Left | Right | Behind | Front | Above | Below
and shape = Shape of string
and color = Color of string
and adjacents = Adjacents of (direction * entity) list
and entity = Entity of shape * color * adjacents
and model = entity list
;;


let perform (s : string) (m : model) =
  "something"

