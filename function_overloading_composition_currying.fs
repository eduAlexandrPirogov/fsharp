// 20.3.1
let vat n x = x + x / 100.0 * float(n) : float

// 20.3.2
let unvat n x = float(n) * 100.0/(100.0+float(x))

// 20.3.3
let rec min f = 
    let rec subn n = f n
    function
    | n when n <= 1 -> n :int
    | n -> min f n-1:int