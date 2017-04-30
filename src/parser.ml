open Model

let parse (s : string) =
  match s with
  | "create 1" -> Create (Entity (!(genid ()), Cube, Red), [])
  | "create 2" -> Create (Entity (!(genid ()), Sphere, Blue), [Adjacent (Right, 1)])
  | "print" -> Print
  | _ -> Error ("Cannot grasp meaning.")
