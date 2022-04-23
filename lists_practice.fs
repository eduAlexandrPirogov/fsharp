// 39.1
let is_even_pos = fun k -> k % 2 = 0
let is_even_elem = fun k -> k % 2 = 0

let rec rmodd = fun list -> 
    let rec fill_list = function
    | (list, k, to_fill) when list = [] -> to_fill
    | (head::tail, k, to_fill) when is_even_pos k  -> fill_list (tail, k+1, head::to_fill)  
    | (head::tail, k, to_fill) -> fill_list (tail, k+1, to_fill)
    List.rev(fill_list (list, 0,[]))


//39.2
let rec del_even = 
    fun list ->
    let rec fill_list = function
        | (list, to_fill) when list = [] -> to_fill
        | (head::tail, to_fill) when is_even_elem head -> fill_list(tail, head::to_fill)
        | (head::tail, to_fill) -> fill_list(tail, to_fill)
    List.rev(fill_list(list, []))


// 39.3. Напишите функцию multiplicity x xs, 
let rec multiplicity x xs= 
    let rec count = function
        | (x, xs, cnt) when xs = [] -> cnt
        | (x, head::tail, cnt) when x = head -> count (x, tail, cnt+1)
        | (x, head::tail, cnt) -> count (x, tail, cnt)
    count(x, xs, 0)


// 39.4
let rec split = fun list ->
    let rec split1 = function
        | (list, k, to_fill1, to_fill2) when list = [] -> (List.rev(to_fill1), List.rev(to_fill2))
        | (head::tail, k, to_fill1, to_fill2) when is_even_pos k -> split1(tail, k+1, head::to_fill1, to_fill2)
        | (head::tail, k, to_fill1, to_fill2) -> split1(tail, k+1, to_fill1, head::to_fill2)
    split1 (list, 0, [], [])

exception RaiseNotEquas
// 39.5
let rec zip (xs1,xs2) = 
    if List.length(xs1) <> List.length(xs2) then raise RaiseNotEquas
    let rec iterate = function
        | (xs1, xs2, to_fill) when xs1 = [] -> to_fill
        | (head1::tail1, head2::tail2, to_fill) -> iterate(tail1, tail2, (head1,head2)::to_fill)
    List.rev(iterate (xs1, xs2, []))