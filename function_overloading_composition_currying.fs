// 20.3.1
let vat n x = x + x / 100.0 * float(n) : float

// 20.3.2
let unvat n x = x * 100.0/(100.0+float(n))

// 20.3.3
let rec min f= 
    let my_n = fun n -> f n
    let start = 0
    let rec rec_check = function 
        | (my_n, start) when my_n start = 0 -> start
        | (my_n, start) -> (rec_check (my_n, start+1))
    rec_check (my_n, start)