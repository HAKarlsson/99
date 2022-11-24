let rec rev_append xs ys =
    match xs with
    | [] -> ys
    | x::xs -> rev_append xs (x::ys)

let remove_at i xs = 
    let rec aux acc i = function
        | [] -> rev_append acc []
        | x::xs -> if i = 0 then rev_append acc xs 
                   else aux (x::acc) (i-1) xs
    in aux [] i xs

let l = ["a"; "b"; "c"; "d"]
let l' = ["a"; "c"; "d"]
let () = assert(remove_at 1 l = l')
