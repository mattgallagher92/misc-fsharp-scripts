let rec naiveFib n =
    match n with
    | 0 | 1 -> 1
    | _ -> naiveFib (n - 1) + naiveFib (n - 2)

naiveFib 45

let tailRecursiveFib n =
    let rec fibt termMMinus1 termM m =
        if m = n then termM
        else fibt termM (termMMinus1 + termM) (m + 1)

    fibt 1 1 1

tailRecursiveFib 45

let foldFib n =
    let _, an = List.fold (fun (a'', a') _i -> (a', a'' + a')) (1, 1) [ 2 .. n ]
    an

foldFib 45

let unfoldFib n =
    List.unfold
        (fun (a'', a', i) ->
            if i = n then None
            else Some (a'' + a', (a', a'' + a', i + 1)))
        (1, 1, 1)
    |> List.rev
    |> List.head

unfoldFib 45
