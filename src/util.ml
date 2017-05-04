(* Exceptions *)
exception No_such_quantifier_exception
exception No_such_adjacent_exception
exception No_such_direction_exception
exception No_such_shape_exception
exception No_such_color_exception
exception No_such_command_exception
exception Tree_not_found
exception Not_a_leaf
exception Incorrect_determiner
exception Not_enough_elements
exception No_such_entity_exception of string



type ansi_color =  AnsiBlack | AnsiRed | AnsiGreen | AnsiOrange | AnsiBlue | AnsiPurple
                  | AnsiCyan | AnsiBoldGrey | AnsiDarkGrey | AnsiBoldRed | AnsiBoldGreen | AnsiYellow
                  | AnsiBoldBlue | AnsiBoldPurple | AnsiBoldCyan | AnsiWhite

let print_color (s : string) (c : ansi_color) =
  let code = match c with
    | AnsiBlack -> "\x1b[30m"
    | AnsiRed -> "\x1b[31m"
    | AnsiGreen -> "\x1b[32m"
    | AnsiOrange -> "\x1b[33m"
    | AnsiBlue -> "\x1b[34m"
    | AnsiPurple -> "\x1b[35m"
    | AnsiCyan -> "\x1b[36m"
    | AnsiBoldGrey -> "\x1b[37m"
    | AnsiDarkGrey -> "\x1b[1;30m"
    | AnsiBoldRed -> "\x1b[1;31m"
    | AnsiBoldGreen -> "\x1b[1;32m"
    | AnsiYellow -> "\x1b[1;33m"
    | AnsiBoldBlue -> "\x1b[1;34m"
    | AnsiBoldPurple -> "\x1b[1;35m"
    | AnsiBoldCyan -> "\x1b[1;36m"
    | AnsiWhite -> "\x1b[1;37m"
  in
  Printf.printf "%s%s%s" code s "\x1b[0m"
