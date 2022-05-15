// 43.3
let try_find key m = 
    let list = Map.toList m
    let rec iter = function
        | (_, [ ]) -> None
        | (key, head::tail) -> 
            let (head1,_) = head
            let (_, tail1) = head
            if head1 = key then
                Some(tail1)
            else
                iter(key, tail)
    iter(key, list)

(*printfn "%b" (Option.get (try_find 128 map1) = "oksana")
printfn "%b" (Option.get (try_find 5 map1) = "dqw")
printfn "%b" (Option.get (try_find 13 map1) = "QQ")
printfn "%b" ( (try_find 16 map1) = None)
printfn "%b" ((try_find 17 map1) = None)*)