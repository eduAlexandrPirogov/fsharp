// 17.1
let rec pow = function 
| (s, n) when n < 0 -> ""
| (s, 0) -> ""
| (s, 1) -> s
| (s, n) -> s + pow(s, n-1)

// 17.2
let rec isIthChar = function
| (s, n, c) when n < 0 || n >= String.length s -> false
| (s, n, c) when s.[n] = c -> true
| (s, n, c) when s.[n] <> c -> false
 
// 17.3
let rec occFromIth = function
| (s, n, c) when n < 0 || n >= String.length s -> 0
| (s, n, c) when ((String.length s) - 1) = n && s.[n] = c -> 1
| (s, n, c) when ((String.length s) - 1)= n && s.[n] <> c -> 0
| (s, n, c) when s.[n] = c -> 1 + occFromIth(s, n+1, c)
| (s, n, c) when s.[n] <> c -> 0 + occFromIth(s, n+1, c)