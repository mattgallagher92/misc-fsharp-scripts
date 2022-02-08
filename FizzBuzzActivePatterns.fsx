let (|MultipleOf15|MultipleOf5|MultipleOf3|Other|) n =
    match n % 15 with
    | 0 -> MultipleOf15
    | 5 | 10 -> MultipleOf5
    | 3 | 6 | 9 | 12 -> MultipleOf3
    | _ -> Other

let strFor n =
    match n with
    | MultipleOf3 -> "Fizz"
    | MultipleOf5 -> "Buzz"
    | MultipleOf15 -> "Fizzbuzz"
    | Other -> sprintf "%d" n

for i in 1..50 do
    printfn "%s" <| strFor i
