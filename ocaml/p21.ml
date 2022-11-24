let rec rev_append xs ys =
    match xs with
    | [] -> ys
    | x::xs -> rev_append xs (x::ys)

let insert_at x i xs = 
    let rec aux acc i = function
        | [] -> rev_append acc [x]
        | h::t -> if i = 0 then rev_append (x::acc) (h::t)
                   else aux (h::acc) (i-1) t
    in aux [] i xs

let l = ["a"; "b"; "c"; "d"]
let l' = ["a"; "alpha";"b" ;"c"; "d"]
let () = assert(insert_at "alpha" 1 l = l')
