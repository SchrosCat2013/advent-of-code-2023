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
