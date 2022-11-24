let split xs n = 
    let rec aux acc i = function
        | [] -> (List.rev acc, [])
        | x::xs -> if i <= 0 then (List.rev acc, x::xs)
                   else aux (x::acc) (i-1) xs
    in aux [] n xs

let l = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"]
let l' = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
let () = assert(split [] 3 = ([],[]))
let () = assert(split [1] 3 = ([1],[]))
let () = assert(split [1;2;3] 3 = ([1;2;3],[]))
let () = assert(split [1;2;3;4] 3 = ([1;2;3],[4]))
let () = assert(split [1;2;3;4] 0 = ([],[1;2;3;4]))
let () = assert(split [1;2;3;4] (-1) = ([],[1;2;3;4]))
let () = assert(split l 3 = l')
