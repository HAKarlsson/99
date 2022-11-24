let rev xs =
    let rec aux acc = function
        | [] -> acc
        | x::xs -> aux (x::acc) xs
    in aux [] xs

let duplicate xs =
    let rec aux acc = function
        | [] -> acc
        | x::xs -> aux (x::x::acc) xs
    in aux [] (rev xs)

let l = ["a"; "b"; "c"; "c"; "d"]
let l' = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
let () = assert(duplicate l = l')
