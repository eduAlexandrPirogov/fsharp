// 41.4.1
let list_filter f xs = List.foldBack(fun elem acc-> if f elem = true then elem::acc else acc@[] ) xs []

// 41.4.2
let sum (p, xs) = List.fold (fun acc elem -> if p elem = true then acc+ elem else acc+0) 0 xs

// 41.4.3
let revrev = function
    | ([]) -> []
    | (xs) -> List.fold (fun acc elem -> (List.fold (fun acc1 elem1 -> elem1::acc1) [] elem)::acc) [] xs
