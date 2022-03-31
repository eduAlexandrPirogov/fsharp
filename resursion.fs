// 7.1.1
let rec fibo = function
| n when n <= 0 -> 0
| 1 -> 1
| n -> fibo(n-1) + fibo (n-2)

// 7.1.2
let rec sum = function
 | n when n <= 1 -> 1
 | n -> n + sum(n-1)

 // 7.1.3
let rec sum2= function
| (m, 0) -> m
| (m, n) -> m + n + sum2(m, n-1)



let rec factorial = function
 | n when n <= 0 -> 1
 | n -> n * factorial(n-1)

printfn "%d" (factorial -1)

printfn "fibonachi"
let rec fibo = function
| n when n <= 0 -> 0
| 1 -> 1
| n -> fibo(n-1) + fibo (n-2)

printfn "%d" (fibo -1)
printfn "%d" (fibo 0)
printfn "%d" (fibo 1)
printfn "%d" (fibo 2)
printfn "%d" (fibo 3)
printfn "%d" (fibo 4)
printfn "%d" (fibo 15)



printfn "Summ2"
let rec sum2= function
| (m, 0) -> 0
| (m, n) -> m + sum2(m, n-1)


printfn "%d" (sum2(1, 1))
printfn "%d" (sum2(2, 2))
printfn "%d" (sum2(3, 3))
printfn "%d" (sum2(4, 5))
printfn "%d" (sum2(6, 6))
printfn "%d" (sum2(5, 5))






printfn ""
printfn "Эталонное решение с сервера"

let rec sum1 = function 
  | 1 -> 1 
  | n when n < 1 -> 0 
  | n -> sum1(n-1) + n 

printfn "sum = %d" (sum1 0)

printfn ""
printfn "Pirogov"
let rec sum = function
  | n when n <= 1 -> 1
  | n -> n + sum(n-1)

printfn "sum = %d" (sum 0)
