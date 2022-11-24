let compress xs =
    let rec aux h = function
        | [] -> []
        | x::xs -> if x = h then aux h xs else x::xs
    in let rec aux' acc = function
        | [] -> List.rev acc
        | x::xs -> aux' (x::acc) (aux x xs)
    in aux' [] xs

let l = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
let l' = ["a"; "b"; "c"; "a"; "d"; "e"]
let () = assert(compress l = l')
