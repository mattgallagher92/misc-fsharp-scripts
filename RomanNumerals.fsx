let charVal = function
    | 'M' -> 1000 | 'D' -> 500 | 'C' -> 100 | 'L' -> 50 | 'X' -> 10 | 'V' -> 5 | 'I' -> 1
    | c -> failwithf $"Unrecognised roman numeral: %c{c}"

let parse (s : string) =
    let cs = List.ofSeq s // [ 'X'; 'C'; 'V'; 'I' ]

    let contributionFromLast =
        List.last cs // 'I'
        |> charVal // 1

    let contributionFromOthers =
        let contributionFor (i, j) = if j >= i then j else -j

        cs
        |> List.rev // [ 'I'; 'V'; 'C'; 'X' ]
        |> List.map charVal // [ 1; 5; 100; 10 ]
        |> List.pairwise // [ 1, 5; 5, 100; 100, 10; ]
        |> List.map contributionFor // [ 5; 100; -10 ]
        |> List.sum // 95

    contributionFromLast + contributionFromOthers

parse "XCVI"