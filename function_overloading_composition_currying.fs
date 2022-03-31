// 20.3.1
let vat n x =  x + x / 100.0 * float(n)

// 20.3.2
let unvat n x =  x * 100.0/(100.0+float(n))

// 20.3.3
let rec min (f: int->int) = fun n ->
    if f n <= 0 then n
    else 
        let f1 n = f n-1
        min f1 n