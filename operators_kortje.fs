// 23.4.1
let (.+.) x y = 
    let ((_,_,c1):int*int*int) = y
    let ((_,s1,_):int*int*int) = y 
    let ((g1,_,_):int*int*int) = y
    let ((_,_,c2):int*int*int) = x
    let ((_,s2,_):int*int*int) = x 
    let ((g2,_,_):int*int*int) = x

    let money = (g1+g2, s1+s2, c1+c2)

    let rec convert_coupers = function
        | (g, s, c) when c <= 11 -> (g,s,c)
        | (g, s, c) -> convert_coupers (g, s+1, c-12)

    let rec convert_silvers = function
        | (g, s, c) when s <= 19 -> (g,s,c)
        | (g, s, c) -> convert_silvers (g+1, s-20, c)


    let coupers_converted = convert_coupers money
    let silvers_converted = convert_silvers coupers_converted
    silvers_converted

let (.-.) x y = 
    let ((_,_,c1):int*int*int) = y
    let ((_,s1,_):int*int*int) = y 
    let ((g1,_,_):int*int*int) = y
    let ((_,_,c2):int*int*int) = x
    let ((_,s2,_):int*int*int) = x 
    let ((g2,_,_):int*int*int) = x

    let money = (g2-g1, s2-s1, c2-c1)

    let rec convert_coupers = function
        | (g, s, c) when c <= 11 -> (g,s,c)
        | (g, s, c) -> convert_coupers (g, s+1, c-12)

    let rec convert_silvers = function
        | (g, s, c) when s <= 19 -> (g,s,c)
        | (g, s, c) -> convert_silvers (g+1, s-20, c)


    let coupers_converted = convert_coupers money
    let silvers_converted = convert_silvers coupers_converted
    silvers_converted

// 23.4.2
let (.+) x y = 
    let (a, _) = x
    let (_, b) = x
    let (c, _) = y
    let (_, d) = y
    (a + c, b + d)


let (.-) x y = 
    let (a, _) = x
    let (_, b) = x
    let (c, _) = y
    let (_, d) = y
    (a-c,b-d)

let (.*) x y = 
    let (a, _) = x
    let (_, b) = x
    let (c, _) = y
    let (_, d) = y
    (a*c - b*d, b *c + a * d)


let (./) x y = 
    let (a, _) = x
    let (_, b) = x
    let (c, _) = y
    let (_, d) = y
    ((a*c+b*d)/(c*c+d*d), (b*c-a*d)/(c*c+d*d))
