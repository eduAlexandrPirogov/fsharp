// 34.1
let rec upto = fun n ->
    let rec fill_list = function 
       | (list, k) when n = k || k < 0 -> list
       | (list, k) -> fill_list ((n-k)::list, k+1)
    fill_list ([], 0)

// 34.2
let rec dnto = fun n ->
    let rec fill_list = function 
       | (list, k) when n+1 = k || k < 0 -> list
       | (list, k) -> fill_list (k::list, k+1)
    fill_list ([], 1)

// 34.3
let rec evenn = fun n ->
    let rec fill_list = function 
       | (list, k) when  k < 0 -> list
       | (list, k) -> fill_list (k*2::list, k-1)
    fill_list ([], n)
