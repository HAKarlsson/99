type 'a node =
    | One of 'a
    | Many of 'a node list

let rev xs =
    let rec aux acc = function
        | [] -> acc
        | x::xs -> aux (x::acc) xs
    in aux [] xs

let rec flatten xs =
    let rec aux acc = function
        | [] -> acc
        | (One x::xs) -> aux (x::acc) xs
        | (Many ys::xs) -> (aux (aux acc ys) xs)
    in rev (aux [] xs)

let l = [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]]
let l' = ["a";"b";"c";"d";"e"]

let () = assert (flatten [] = [])
let () = assert (flatten l = l')
