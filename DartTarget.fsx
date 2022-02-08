// Inspired by https://nrich.maths.org/12261

type ListBuilder() =
    member this.Bind(list, f) = list |> List.collect f
    member this.Return(x) = [x]

let list = ListBuilder()

let allZones = [ 1; 1; 1; 2; 2; 2; 3; 3; 3; 4; 4; 5; 5 ]

let allStrictlyOrderedQuadruples = list {
    let! i = [ 0 .. 11 ]
    let! j = [ (i + 1) .. 11 ]
    let! k = [ (j + 1) .. 11 ]
    let! l = [ (k + 1) .. 11 ]
    return i, j, k, l
}

// Expect 12C4 = 495
List.length allStrictlyOrderedQuadruples

let possibleGames =
    allStrictlyOrderedQuadruples
    |> List.map (fun (i, j, k, l) -> let itemN n = List.item n allZones in itemN i, itemN j, itemN k, itemN l)
    |> List.distinct

let score (i, j, k, l) = i + j + k + l

possibleGames
|> List.filter (fun g -> let s = score g in s = 11 || s = 12 || s = 13)
|> List.groupBy score