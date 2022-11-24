let rev xs =
    let rec aux acc = function
        | [] -> acc
        | x::xs -> aux (x::acc) xs
    in aux [] xs

let drop xs n =
    let rec aux i acc = function
        | [] -> acc
        | x::xs -> if i = n then aux 1 acc xs 
                   else aux (i+1) (x::acc) xs
    in rev (aux 1 [] xs)

let l = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"];;
let l' = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
let () = assert(drop l 3 = l')
