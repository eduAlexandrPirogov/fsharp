//40.1
let rec sum (p, xs) = 
    let rec sum_elem = function
    | (p, [], k) -> 0
    | (p, head::tail, k) when tail = [] && p head -> k+head
    | (p, head::tail, k) when tail = [] -> k
    | (p, head::tail, k) when p head = true -> sum_elem(p, tail, head + k)
    | (p, head::tail, k) -> sum_elem(p, tail, k)
    sum_elem (p, xs,0)

//40.2.1
let rec count (xs, n) = 
    let rec fill_k = function
        | ([], n, k) -> 0
        | (head::tail, n, k) when head > n-> k
        | (head::tail, n, k) when head = n -> fill_k(tail, n, k+1)
        | (head::tail, n, k) -> fill_k(tail, n, k)
    fill_k(xs, n, 0)

//40.2.2
let rec insert (xs, n) = 
    let rec insert_list = function
    | ([], n, res) -> res @ n::[]
    | (head::tail, n, res) when head = n -> res @ n::[] @ head::[] @ tail //inside list
    | (head::tail, n, res) when head > n -> res @ n::[] @ head::[] @ tail //the biggest onelist
    | (head::tail, n, res) when head < n && tail = [] -> res @ head::[] @ n::[] @ tail //the smallest one
    | (head::tail, n, res) -> insert_list(tail, n, res @ head::[] )
    insert_list(xs, n, [])

let rec dublicate = function
    | ([], k) -> false
    | (head::tail, (k:int)) when tail= [] -> false
    | (head::tail, k) when head = k -> true
    | (head::tail, k) -> dublicate(tail, k)

let rec insert_wo_dubl = function
    | ([], n, pred) -> n ::[]
    | (head::tail, (n:int), pred) when pred (head::tail, n) = true -> []
    | (head::tail, (n:int), pred) when pred (head::tail, n) = false ->n::[]

let rec check_in = function
| ([], n) -> false
| (head::tail, n) when tail = [] && head = n -> true //if last item = n
| (head::tail, n) when tail = [] && head <> n -> false 
| (head::tail, n) when head = n -> true
| (head::tail, n) -> check_in (tail, n)

// 40.2.3
let rec intersect (xs1, xs2) = 
    let rec fill_intersect = function
        | ([], xs2, res) -> []
        | (head1::tail1, xs2, res) when tail1 = [] && check_in (xs2,head1) = false -> res //Если нет во втором списк вообще
        | (head1::tail1, xs2, res) when tail1 = [] && check_in (xs2,head1) = true && check_in (res, head1) = false-> res @ head1::[] 
        | (head1::tail1, xs2, res) when tail1 = [] && check_in (xs2,head1) = true && check_in (res, head1) = true-> res @ head1::[]
        | (head1::tail1, xs2, res) when check_in (xs2,head1) = true && check_in(res, head1) = true -> fill_intersect(tail1, xs2, res @ head1::[]) // если есть во 2 сп но нет в рез
        | (head1::tail1, xs2, res) when check_in (xs2,head1) = true && check_in(res, head1) = false -> fill_intersect(tail1, xs2, res @ head1::[]) // если есть во 2 сп но нет в рез
        | (head1::tail1, xs2, res) when check_in (xs2,head1) = false -> fill_intersect(tail1, xs2, res)
    if List.length xs1 >= List.length xs2 then
        fill_intersect (xs1, xs2, [])
    else
        fill_intersect (xs2, xs1, [])


//40.2.4
let rec plus (xs1, xs2) = 
    let rec fill_plus = function
        | ([], [], res) -> res
        | ([], head2::tail2, res) -> head2::tail2
        | (head1::tail1, [], res) -> head1::tail1
        | (head1::tail1, head2::tail2, res) when tail1 = [] && tail2 = [] && head1 >= head2 -> res @ head2::[] @ head1::[]
        | (head1::tail1, head2::tail2, res) when tail1 = [] && tail2 = [] && head1 <= head2 -> res @ head1::[] @ head2::[]
        | (head1::tail1, head2::tail2, res) when head1 > head2 && tail2 = [] -> res @ head2::[] @ head1 :: [] @ tail1
        | (head1::tail1, head2::tail2, res) when head1 < head2 && tail1 = [] -> res @ head1::[] @ head2 :: [] @ tail2
        | (head1::tail1, head2::tail2, res) when head1 = head2 && tail1 = []-> res @ head1::[] @ head2::[] @ tail2
        | (head1::tail1, head2::tail2, res) when head1 = head2 && tail2 = []-> res @ head1::[] @ head2::[] @ tail1
        | (head1::tail1, head2::tail2, res) when head1 > head2 -> fill_plus(head1::tail1, tail2, res @ head2::[])
        | (head1::tail1, head2::tail2, res) when head1 < head2 -> fill_plus(tail1, head2::tail2, res @ head1::[])
        | (head1::tail1, head2::tail2, res) when head1 = head2 -> fill_plus(tail1, tail2, res @ head1::[] @ head2::[])
    fill_plus(xs1,xs2,[])

//4.2.5
let rec minus (xs1, xs2) = 
    let rec fill_minus = function
        | ([], xs2, res) -> res
        | (head1::tail1, xs2, res) when tail1 = [] && check_in (xs2,head1) = true -> res
        | (head1::tail1, xs2, res) when tail1 = [] && check_in (xs2,head1) = false-> res @ head1::[]
        | (head1::tail1, xs2, res) when check_in (xs2,head1) = true -> fill_minus(tail1, xs2, res)
        | (head1::tail1, xs2, res) when check_in (xs2,head1) = false -> fill_minus(tail1, xs2, res @ head1::[])
    fill_minus (xs1, xs2, [])


// 40.3.1
let rec smallest = function 
    | ([]) -> None
    | ((head:int)::(head1::tail)) when tail = [] && head1 <= head-> Some(head1)
    | (head::(head1::tail)) when tail = [] && head1 >= head-> Some(head)
    | (head::(head1::tail)) when head > head1 -> smallest(head1::tail)
    | (head::(head1::tail)) -> smallest(head::tail)


// 40.3.2
let rec delete (n, xs) = 
    let rec delete_min = function
        | (n, [], res) -> res
        | ((n:int), head::tail, res) when tail = [] && head = n -> res @ tail
        | (n, head::tail, res) when tail = [] && head <> n -> res @ (head::tail)
        | (n, head::tail, res) when n = head -> res @ tail
        | (n, head::tail, res) -> delete_min(n, tail, res @ head::[])
    delete_min(n, xs, [])

let rec sort_helper = function
    | ([], res) -> res
    | (head::tail, res) when tail = [] -> res @ head::[]
    | (head::tail, res) -> sort_helper(delete(Option.get (smallest(head::tail)), head::tail), res @ (Option.get (smallest(head::tail)))::[])

// 40.3.3
let rec sort = fun xs ->
    sort_helper(xs, [])


let min_list_rev xs:int list = List.rev xs
// 40.4
let rec revrev = fun (xs:int list list) ->
    let rec fill = function
        | ([], res) -> res
        | ((head:int list)::(tail:int list list), (res:int list list)) when tail = [] -> List.rev(res @ (min_list_rev head)::[])
        | ((head:int list)::(tail:int list list), (res:int list list)) -> fill(tail, res @ (min_list_rev head)::[])
    fill(xs, [])
