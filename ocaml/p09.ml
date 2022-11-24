let rev xs =
    let rec aux acc = function
        | [] -> acc
        | x::xs -> aux (x::acc) xs
    in aux [] xs

let pack xs =
    let rec aux curr acc = function
        | [] -> ([], acc)
        | x::xs -> if x = curr then aux curr (x::acc) xs
                   else (x::xs, acc) 
    in let rec aux' acc = function
        | [] -> acc
        | x::xs ->
                let (xs',grp) = aux x [x] xs
                in aux' (grp::acc) xs'
    in aux' [] (rev xs)

let l = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]
let l' = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]

let () = assert (pack [] = [])
let () = assert (pack ["a"] = [["a"]])
let () = assert (pack l = l')
