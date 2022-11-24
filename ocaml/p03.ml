let rec nth xs n = 
    match xs with
    | [] -> None
    | x::xs -> if n = 0 then Some x else nth xs (n-1)

let () = assert(nth [1] 1 = None)
let () = assert(nth [] 0 = None)
let () = assert(nth [] 2 = None)
let () = assert(nth [] (-2) = None)
let () = assert(nth [0;1;2;3;4] (-2) = None)
let () = assert(nth [0;1;2;3;4] 0 = Some 0)
let () = assert(nth [0;1;2;3;4] 2 = Some 2)
let () = assert(nth [0;1;2;3;4] 4 = Some 4)
