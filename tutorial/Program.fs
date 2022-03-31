


    

let h2 = ((fun y -> y - 5)) << (fun z -> z * z * z)


let fn x = fun y z -> y - x - z
let minus10 = fn 10
printfn "%d" (fn 2 3 3)
printfn "%d" (fn 10 12 4)
printfn "%d" (minus10 12 -5)


printfn "______________"
// 20.3.1
let vat n x =  x + x / 100.0 * float(n) : float

let vat_res = vat 50 200
printfn "vat_res = %f" vat_res

// 20.3.2
let unvat n x =  float(x) * 100.0/(100.0+float(n))
let unvat_res = unvat 50 200.0
printfn "unvat_res = %f" unvat_res

let res n = (unvat n (vat n 200.0))

printfn"______"
printfn "unvat vat = %f" (res 50)

// 20.3.3


let f0 = fun x -> x - 5

let f1 = fun y -> y-5

let f22 (f: int-> int) = fun n -> f n:int

let xx n = f0 n


// 20.3.3
let rec min (f: int->int) = fun n ->
    if f n <= 0 then n
    else 
        let f1 n = f n-1
        min f1 n
    
let lam_f = fun n -> n
printfn "%d" (min f0 16)
