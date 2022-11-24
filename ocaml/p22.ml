let range i k =
    let rec aux acc i k = 
        if i = k then i::acc
        else aux (k::acc) i (k-1)
    in if i <= k then aux [] i k else []

let () = assert(range 4 4 = [4])
let () = assert(range 4 1 = [])
let () = assert(range 4 9 = [4;5;6;7;8;9])
