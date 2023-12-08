open System.Text.RegularExpressions

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
