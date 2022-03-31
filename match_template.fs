let days_in_month = function
| 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
| 4 | 6 | 9| 11 -> 30
| 2 -> 28
| _ -> 0




function 
| "null" -> 0
| "plus" -> +1
| "minus" -> -1

let text2int = function
| "null" -> 0
| "plus" -> +1
| "minus" -> -1

let int2text = function
| 1 -> "one"
| 2 -> "two"
| 3 -> "three"
| _ -> "idk..."

let dishes = function
| "borsh"|"uha"|"solyanka"  -> "sup"
| "rus"|"pure"|"ratatui"    -> "garnir"
| _                         -> "new"


printfn "%d" (text2int "null")
printfn "%s" (int2text 1)
printfn "%s" (int2text 2)
printfn "%s" (int2text 3)
printfn "%s" (int2text 4)
printfn "%s" (dishes "uha")


let calculate_speed t s = t / s;
printfn "%f" (calculate_speed 5. 10.0)

let mul2 n y = function
| 0 -> 1
| n -> n * y

printfn "%d" (mul2 3 4 5)


let match_use n = match n with
| 0 -> 1
| n -> n * 2

let match_use_or n = match n with
| "Alex"|"Anton" -> "male"
| "Sasha"|"Kate" -> "female"

printfn "%d" (match_use 30)
printfn "%s" (match_use_or "Anton")

let when_use n = match n with
| 0 -> 1
| n when n < 10 -> n * 2
| n when (n > 10 && n < 20) -> n * 3
| _ -> n * 100

printfn "Using when_use"
printfn "%d" (when_use 0)
printfn "%d" (when_use 5)
printfn "%d" (when_use 15)
printfn "%d" (when_use 20)

let days_in_month = function
| 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
| 4 | 6 | 9| 11 -> 30
| 2 -> 28
| _ -> 0

let month_to_int n = match n with
| "������"   -> 1
| "�������"  -> 2
| "����"     -> 3
| "������"   -> 4
| "���"      -> 5
| "����"     -> 6
| "����"     -> 7
| "������"   -> 8
| "��������" -> 9
| "�������"  -> 10
| "������"   -> 11
| "�������"  -> 12
| _          -> -1

printfn "������ %d" (days_in_month (month_to_int "������"))
printfn "������� %d" (days_in_month (month_to_int "�������"))
printfn "���� %d" (days_in_month (month_to_int "����"))
printfn "������ %d" (days_in_month (month_to_int "������"))
printfn "��� %d" (days_in_month (month_to_int "���"))
printfn "���� %d" (days_in_month (month_to_int "����"))
printfn "���� %d" (days_in_month (month_to_int "����"))
printfn "������ %d" (days_in_month (month_to_int "������"))
printfn "�������� %d" (days_in_month (month_to_int "��������"))
printfn "������� %d" (days_in_month (month_to_int "�������"))
printfn "������ %d" (days_in_month (month_to_int "������"))
printfn "������� %d" (days_in_month (month_to_int "�������"))
printfn "Tess %d" (days_in_month (month_to_int "qwe"))

printfn "Test %d" (days_in_month -1)
printfn "Test %d" (days_in_month 0)
printfn "Test %d" (days_in_month 13)



function 
| "null" -> 0
| "plus" -> +1
| "minus" -> -1

let text2int = function
| "null" -> 0
| "plus" -> +1
| "minus" -> -1

let int2text = function
| 1 -> "one"
| 2 -> "two"
| 3 -> "three"
| _ -> "idk..."

let dishes = function
| "borsh"|"uha"|"solyanka"  -> "sup"
| "rus"|"pure"|"ratatui"    -> "garnir"
| _                         -> "new"


printfn "%d" (text2int "null")
printfn "%s" (int2text 1)
printfn "%s" (int2text 2)
printfn "%s" (int2text 3)
printfn "%s" (int2text 4)
printfn "%s" (dishes "uha")


let calculate_speed t s = t / s;
printfn "%f" (calculate_speed 5. 10.0)

let mul2 n y = function
| 0 -> 1
| n -> n * y

printfn "%d" (mul2 3 4 5)


let match_use n = match n with
| 0 -> 1
| n -> n * 2

let match_use_or n = match n with
| "Alex"|"Anton" -> "male"
| "Sasha"|"Kate" -> "female"

printfn "%d" (match_use 30)
printfn "%s" (match_use_or "Anton")

let when_use n = match n with
| 0 -> 1
| n when n < 10 -> n * 2
| n when (n > 10 && n < 20) -> n * 3
| _ -> n * 100

printfn "Using when_use"
printfn "%d" (when_use 0)
printfn "%d" (when_use 5)
printfn "%d" (when_use 15)
printfn "%d" (when_use 20)

let days_in_month = function
| 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
| 4 | 6 | 9| 11 -> 30
| 2 -> 28
| _ -> 0

let month_to_int n = match n with
| "������"   -> 1
| "�������"  -> 2
| "����"     -> 3
| "������"   -> 4
| "���"      -> 5
| "����"     -> 6
| "����"     -> 7
| "������"   -> 8
| "��������" -> 9
| "�������"  -> 10
| "������"   -> 11
| "�������"  -> 12
| _          -> -1

printfn "������ %d" (days_in_month (month_to_int "������"))
printfn "������� %d" (days_in_month (month_to_int "�������"))
printfn "���� %d" (days_in_month (month_to_int "����"))
printfn "������ %d" (days_in_month (month_to_int "������"))
printfn "��� %d" (days_in_month (month_to_int "���"))
printfn "���� %d" (days_in_month (month_to_int "����"))
printfn "���� %d" (days_in_month (month_to_int "����"))
printfn "������ %d" (days_in_month (month_to_int "������"))
printfn "�������� %d" (days_in_month (month_to_int "��������"))
printfn "������� %d" (days_in_month (month_to_int "�������"))
printfn "������ %d" (days_in_month (month_to_int "������"))
printfn "������� %d" (days_in_month (month_to_int "�������"))
printfn "Tess %d" (days_in_month (month_to_int "qwe"))

printfn "Test %d" (days_in_month -1)
printfn "Test %d" (days_in_month 0)
