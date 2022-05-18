// 47.4.1
let f n = 
   let mutable count = n
   let mutable result = 1;
   while count > 0 do
      result <- result * count 
      count <- count - 1
   let fin = result
   fin

// 47.4.2
let fibo n = 
    let mutable count = 0
    let mutable result = 0
    let mutable result1 = 1
    while count < n do
       result <- result + result1
       result1 <- result - result1
       count <- count + 1
    let fin = result
    fin
