﻿let even_seq = Seq.initInfinite (fun i -> i*2)



let fac_seq = 
    Seq.initInfinite (fun i -> 
    let rec fact n acc = 
        if n <= 1 then acc
        else fact (n-1) (acc*n)
    fact i 1)


// 49.5.3
let seq_seq = Seq.initInfinite (fun i -> if i % 2 <> 0 then  (i+1)/2 * (-1) else i/2)
(*
printfn "%b" (Seq.nth 0 seq_seq = 0)
printfn "%b" (Seq.nth 1 seq_seq = -1)
printfn "%b" (Seq.nth 2 seq_seq = 1)
printfn "%b" (Seq.nth 3 seq_seq = -2)
printfn "%b" (Seq.nth 4 seq_seq = 2)
printfn "%b" (Seq.nth 5 seq_seq = -3)
printfn "%b" (Seq.nth 6 seq_seq = 3)
printfn "%b" (Seq.nth 7 seq_seq = -4)
printfn "%b" (Seq.nth 8 seq_seq = 4)
printfn "%b" (Seq.nth 9 seq_seq = -5)
printfn "%b" (Seq.nth 10 seq_seq = 5)*)

(*printfn "%b" (Seq.nth 0 even_seq = 0)
printfn "%b" (Seq.nth 1 even_seq = 2)
printfn "%b" (Seq.nth 2 even_seq = 4)
printfn "%b" (Seq.nth 3 even_seq = 6)
printfn "%b" (Seq.nth 4 even_seq = 8)
printfn "%b" (Seq.nth 5 even_seq = 10)
printfn "%b" (Seq.nth 6 even_seq = 12)
printfn "%b" (Seq.nth 7 even_seq = 14)*)

(*printfn "%b" (Seq.nth 0 fac_seq = 1)
printfn "%b" (Seq.nth 1 fac_seq = 1)
printfn "%b" (Seq.nth 2 fac_seq = 2)
printfn "%b" (Seq.nth 3 fac_seq = 6)
printfn "%b" (Seq.nth 4 fac_seq = 24)
printfn "%b" (Seq.nth 5 fac_seq = 120)
printfn "%b" (Seq.nth 6 fac_seq = 720)
*)