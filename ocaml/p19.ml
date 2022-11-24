let rotate xs n = 
    let len = List.length xs in
    let n' = (((n mod len) + len) mod len) in
    let rec aux acc i = function
        | [] -> List.rev acc
        | x::xs -> if i <= 0 then (x::xs) @ (List.rev acc) 
                   else aux (x::acc) (i-1) xs
    in aux [] n' xs

let l = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"]
let l' = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]
let () = assert(rotate l 3 = l')
let () = assert(rotate l (-5) = l')
