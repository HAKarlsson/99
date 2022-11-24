let rec last_two = function
    | [] -> None
    | [x;y] -> Some (x,y)
    | _::xs -> last_two xs

let () = assert(last_two [] = None)
let () = assert(last_two [0] = None)
let () = assert(last_two [0;1;2;3;4] = Some (3,4))
