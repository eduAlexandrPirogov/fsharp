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