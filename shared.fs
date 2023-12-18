open System.Text.RegularExpressions
open System.Collections.Generic

module Shared =
    [<Struct>]
    type Point = {
        x: int
        y: int
    }

    let splitBy<'T when 'T : equality> (separator: 'T) (source: 'T seq) =
        use enumerator = source.GetEnumerator()
        seq {
            let mutable cont = enumerator.MoveNext()
            while cont do
                yield seq {
                    while cont && not (enumerator.Current = separator) do
                        yield enumerator.Current
                        cont <- enumerator.MoveNext()
                } |> Array.ofSeq

                if cont then
                    cont <- enumerator.MoveNext()
                    if not cont then yield [||]
        }

    // TODO - it would be good to get this to use an IAsyncEnumerable
    // and parse each line as it comes in
    let readAndParseInput<'T> (filename: string) (parser: string -> 'T) =
        File.ReadAllLinesAsync filename
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> Array.map parser

    let transposeSequences<'T> (input: 'T seq seq) =
        let rec next (enumerators: IEnumerator<'T> array) = seq {
            if enumerators |> Array.forall (fun e -> e.MoveNext())
            then
                yield enumerators |> Array.map (fun e -> e.Current)
                yield! next enumerators
        }

        seq {
            let enumerators =
                input
                |> Seq.map (fun x -> x.GetEnumerator())
                |> Array.ofSeq

            try
                yield! next enumerators
            finally
                enumerators |> Array.iter (fun e -> e.Dispose())
        }

    let memoize f =
        let dict = Dictionary<_, _>();
        fun c ->
            let exist, value = dict.TryGetValue c
            match exist with
            | true -> value
            | _ -> 
                let value = f c
                dict.Add(c, value)
                value

module Seq =
    let private miniMaxiFolder<'T> (op: 'T->'T->bool) (index: int, mIndex: int, mValOpt: 'T option) (v: 'T) =
        match mValOpt with
        | Some mVal ->
            if op v mVal
            then (index+1, index, Some v)
            else (index+1, mIndex, Some mVal)
        | None -> (index + 1, index, Some v)

    let maxi<'T when 'T: comparison> (s: 'T seq) =
        match s |> Seq.fold (miniMaxiFolder (>)) (0, 0, None) with
        | (_, i, Some v) -> Some (i, v)
        | _ -> None
    let mini<'T when 'T: comparison> (s: 'T seq) =
        match s |> Seq.fold (miniMaxiFolder (<)) (0, 0, None) with
        | (_, i, Some v) -> Some (i, v)
        | _ -> None

module List =
    let rec comb n l = 
        match n, l with
        | 0, _ -> [[]]
        | _, [] -> []
        | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

module Array2D =
    let tryGet<'T> (y: int) (x: int) (array: 'T[,]) =
        if y >= 0 && x >= 0 && y < (Array2D.length1 array) && x < (Array2D.length2 array)
        then Some array[y, x]
        else None

    let rows<'T> (array: 'T[,]) =
        let (length1, length2) = (Array2D.length1 array, Array2D.length2 array)
        { 0 .. length1 - 1 }
        |> Seq.map (fun y -> Array.init length2 (fun x -> array[y, x]))

    let renderChars (array: char[,]) =
        let sb = StringBuilder()
        for y = 0 to Array2D.length1 array - 1 do
            for x = 0 to Array2D.length2 array - 1 do
                sb.Append array[y,x]
            sb.AppendLine()

        sb.ToString()

    let foldi (folder: int -> int -> 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
        let mutable state = state
        for y in 0 .. Array2D.length1 array - 1 do
            for x in 0 .. Array2D.length2 array - 1 do
                state <- folder y x state (array.[y, x])
        state

    let rec private tryFindIndexImpl<'T> (predicate: 'T -> bool) (y: int) (x: int) (array: 'T[,]) =
        if (predicate array[y,x])
        then Some (y, x)
        else if y + 1 >= Array2D.length1 array && x + 1 >= Array2D.length2 array
        then None
        else if x + 1 >= Array2D.length2 array
        then tryFindIndexImpl predicate (y+1) 0 array
        else tryFindIndexImpl predicate y (x+1) array
    
    let tryFindIndex<'T> (predicate: 'T -> bool) (array: 'T[,]) =
        tryFindIndexImpl predicate 0 0 array

    let countWherei<'T> (predicate: int -> int -> 'T -> bool) (array: 'T[,]) =
        let mutable count = 0
        for y = 0 to Array2D.length1 array - 1 do
            for x = 0 to Array2D.length2 array - 1 do
                if predicate y x array[y, x]
                then count <- count + 1
        
        count

    let countWhere<'T> (predicate: 'T -> bool) (array: 'T[,]) =
        countWherei (fun y x -> predicate) array
