let rev xs =
    let rec aux acc = function
        | [] -> acc
        | x::xs -> aux (x::acc) xs
    in aux [] xs

let replicate xs n =
    let rec aux x acc = function
        | 0 -> acc
        | n -> aux x (x::acc) (n-1)
    in let rec aux' acc = function
        | [] -> acc
        | x::xs -> aux' (aux x acc n) xs
    in aux' [] (rev xs)

let l = ["a"; "b"; "c"]
let l' = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
let () = assert(replicate l 3 = l')
