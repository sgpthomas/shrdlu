open Model

let parse (s : string) =
  match s with
  | "create" -> Create (Entity (Cube, Red, []), [])
  | "print" -> Print
  | _ -> Error ("Cannot grasp meaning.")
