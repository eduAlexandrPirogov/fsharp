// 42.3
let rec allSubsets n k = 
    let rec getAllSubsetes = function
        | ([]) -> [[]]
        | (head::tail) ->  List.fold (fun listOfLists sublist -> (head :: sublist) :: sublist :: listOfLists) [] (getAllSubsetes tail)
    let filterBySize xs k = List.filter (fun (list:'a list) -> list.Length = k) xs
    Set.ofList (List.map (fun xs -> Set.ofList xs) (filterBySize (getAllSubsetes [ 1 .. n ]) k))


// [1;2;3]
// given set sizeofsubset start
// b = [ [1]; [2]; [3]; [1;2]; [1;3]; [2;3]; [1;2;3]]

// [1;2;3;4;5]
// [1] [2] [3] [4] [5]
// [1;2] [1;3] [1;4] [1;5] [2;3] [2;4] [2;5] [3;4] [3;5] [4;5]
// [1;2;3] [1;2;4] [1;2;5] [1;3;4] [1;3;5] [1;4;5] [2;3;4] [2;3;5] [2;4;5] [3;4;5]
// [1;2;3;4] [1;2;4;5] [1;3;4;5] 


// [1;] 
// [1;2]
// [1;2;3]
// [1;2;3;4]
// [1;2;3;4;5]

// [1;2;3;4;5;6;7]
// [1;2;3] [1;2;4] [1;2;5] [1;2;6] [1;2;7]
// [1;3;4] [1;3;5] [1;3;6] [1;3;7]
// [1;4;5] [1;4;6] [1;4;7]

// [1;2;3;4;5]
// [1] [2;3;4;5]
// [1;2] [3;4;5]  -- [1;3] [2;4;5] -- [1;4] [2;3;5] -- [1;5] [2;3;4]
// [1;2;3] [4;5]
// [1;2;3;4] [5]

//start setSize subSetSize 

// count subsets of n = 2^n
//for example 2^3 = 8

// [1;2;3;4;5] --> [1;2;3;4;5]
// [1;2]


// [1;2;3;4]

// 1 2 3 4 
// 12 13 14 23 24 34
// 123 124 145 234
// 1234



// 1;2;3;4;5;6
// [1;2] [1;3] [1;4] [1;5] [1;6]
// [1;2;3] [1;3;4] [1;4;5] [1;5;6]

// [1;2;3;4;5;6] 3 2

// 1;2;3;4;5;6 
// 1;2;3;4;5 -- 6
// 1;2;3;4; -- 5;6

//1) get all subsets and filter it

//2) ...




//let a = allSubsets 3 2

// [1;2;3;4;5]

(*let rec getAllSubsetes2 = function
      | ([]) -> [[]]
      | (head::tail) ->  List.fold (fun listOfLists sublist -> (head :: sublist) :: sublist :: listOfLists) [] (getAllSubsetes2 tail)

let res = getAllSubsetes2 [1;2;3]*)

//recursive tree of [1;2;3]

//1) head = 1; tail = [2;3;]

//2) head = 2; tail = [3;]

//3) head = 3; tail = []

//4) List.fold (ys = 0; s = 0

//5) head = 3; tail = 0; _arg1 = [3]
//6) head = 2; tail = [3] _arg1 = [2;3]
//7) ys = 0; s = [3]
//8) ys = [2;1] s = []