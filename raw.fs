type F =
    | AM
    | PM

type TimeOfDay = { hours: int; minutes: int; f: F }

let (.>.) x y = 
    let h1 = x.hours
    let m1 = x.minutes
    let f1 = x.f
    let h2 = y.hours
    let m2 = y.minutes
    let f2 = y.f
    let s = function 
    | (h1,m1,f1), (h2,m2,f2) when f2 < f1 -> true
    | (h1,m1,f1), (h2,m2,f2) when f2 = f1 && h2 < h1 -> true
    | (h1,m1,f1), (h2,m2,f2) when f2 = f1 && h2 = h1 && m2 < m1 -> true
    | _ -> false
    let b = s ((h1,m1,f1), (h2,m2,f2))
    b

let f1 = AM
let f2 = PM

let x = {hours = 11; minutes = 55; f = f1}
let y = {hours = 11; minutes = 55; f = f2}

let res = x .>. y

printfn "%b" res