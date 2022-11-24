let length xs =
    let rec aux acc = function
        | [] -> acc
        | _::xs -> aux (acc+1) xs
    in aux 0 xs

let () = assert(length [] = 0)
let () = assert(length [0] = 1)
let () = assert(length [0;1;2] = 3)
