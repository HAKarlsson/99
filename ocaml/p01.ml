let rec last = function
    | [] -> None
    | [x] -> Some x
    | _::xs -> last xs

let () = assert(last [] = None)
let () = assert(last [0;1;2;3;4] = Some 4)
