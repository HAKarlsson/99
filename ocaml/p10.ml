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
                let (xs',n) = aux x 1 xs
                in aux' ((n,x)::acc) xs'
    in aux' [] (rev xs)

let l = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
let l' = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
let () = assert(encode l = l')
