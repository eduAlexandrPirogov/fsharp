// 16.1
let notDivisible (n, m) =  m % n = 0

printfn "5 is dibisible by 10 %b" (notDivisible(20, 10))

let count = 0

let myy (m,d) = m%d =0 
printfn "%b" (myy (4,2))

let my_incr(param1) = param1 + 1
let my_dicr(param1) = param1 -1

let incr_by (a,b) =  a + b

let rec my_for = function 
    | (var, func, start, finish, step) when start >= finish -> 0
    | (var, func, start, finish, step) -> var+func + my_for(var, func, start+step, finish, step)

let res = my_for(0, (incr_by (5, 2)), 0, 10, 1);

printfn "%d" res
// 16.2
let prime n = 
    let n_end = n
    let n_start = 1
    let my_mod = function 
        | (m, d) when d = 0 -> 0
        | (m, d) when m % d = 0 -> 1
        | _ -> 0
    let rec my_for = function 
    | (var, start, finish, step, func) when start >= finish -> 1
    | (var, start, finish, step, func) -> var + func(n, n_end-start) + my_for(var,  start+step, finish, step, func)
    let count = my_for(0, n_start, n_end, 1, my_mod)
    count <= 2

let bol = prime 1
let bol2 = prime 2
let bol3 = prime 3
let bol5 = prime 5
let bo7 = prime 7
let bo13 = prime 13
let bo14 = prime 997

printfn "%b" bol
printfn "%b" bol2
printfn "%b" bol3
printfn "%b" bol5
printfn "%b" bo7
printfn "%b" bo13
printfn "%b" bo14
let rec for_print = function
    | (var, func, f_param, ff, start, finish, step) when start >= finish ->func(f_param(ff))
    | (var, func, f_param, ff, start, finish, step) -> func(f_param(ff)) + for_print(var, func, f_param, ff, start+step, finish, step)


    



  
    

  
// 16.1
let notDivisible (n, m) =  m % n = 0

// 16.2
let prime n = 
    let n_end = n
    let n_start = 1
    let my_mod = function 
        | (m, d) when d = 0 -> 0
        | (m, d) when m % d = 0 -> 1
        | _ -> 0
    let rec my_for = function 
    | (var, start, finish, step, func) when start >= finish -> 1
    | (var, start, finish, step, func) -> var + func(n, n_end-start) + my_for(var,  start+step, finish, step, func)
    let count = my_for(0, n_start, n_end, 1, my_mod)
    count <= 2



  
    