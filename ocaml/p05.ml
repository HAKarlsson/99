let rev xs =
    let rec aux acc = function
        | [] -> acc
        | x::xs -> aux (x::acc) xs
    in aux [] xs

let () = assert(rev [] = [])
let () = assert(rev [1;2;3] = [3;2;1])
let () = assert(rev [1;2;3;4] = [4;3;2;1])
let () = assert(rev [1] = [1])
