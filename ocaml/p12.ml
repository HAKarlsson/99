type 'a rle =
    | One of 'a
    | Many of int * 'a

let rev xs =
    let rec aux acc = function
        | [] -> acc
        | x::xs -> aux (x::acc) xs
    in aux [] xs

let encode xs =
    let rec aux curr acc = function
        | [] -> ([], acc)
        | x::xs -> if x = curr then aux curr (acc+1) xs
                   else (x::xs, acc) 
    in let rec aux' acc = function
        | [] -> acc
        | x::xs ->
                let (xs',n) = aux x 1 xs in
                let grp = if n = 1 then One x else Many (n, x)
                in aux' (grp::acc) xs'
    in aux' [] (rev xs)

let decode xs =
    let rec aux x acc = function
        | 0 -> acc
        | n -> aux x (x::acc) (n - 1)
    in
    let rec aux' acc = function
        | [] -> acc
        | One x::xs -> aux' (x::acc) xs
        | Many (n, x)::xs -> aux' (aux x acc n) xs
    in aux' [] (rev xs)

let l = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
let l' = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
let () = assert(encode l = l')
let () = assert(decode l' = l)
