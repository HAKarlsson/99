let slice xs i k =
    let rec drop i = function
        | [] -> []
        | x::xs -> if i <= 0 then x::xs
                   else drop (i-1) xs
    and take acc i = function
        | [] -> List.rev acc
        | x::xs -> if i <= 0 then List.rev acc
                   else take (x::acc) (i-1) xs
    in take [] (k - i + 1) (drop i xs)

let l = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"];;
let l' = ["c"; "d"; "e"; "f"; "g"]
let () = assert(slice l 2 6 = l')
