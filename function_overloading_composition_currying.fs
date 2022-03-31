// 20.3.1
let vat n x = x + x / 100.0 * float(n) : float

// 20.3.2
let unvat n x = fun (x:float) -> float(n) * 100.0/(100.0+x)

// 20.3.3
let rec min f = (fun (n:int) -> 
    let n = f n
    match (n) with
    | n when f n <= 0 -> n: int
    | _ -> min f n : int)