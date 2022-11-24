let rev xs =
    let rec aux acc = function
        | [] -> acc
        | x::xs -> aux (x::acc) xs
    in aux [] xs

let is_palindrome xs = (rev xs = xs)

let () = assert(not (is_palindrome [1;2;3;4]))
let () = assert(is_palindrome [1;2;3;4;3;2;1])
