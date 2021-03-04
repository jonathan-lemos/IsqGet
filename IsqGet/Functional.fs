module IsqGet.Functional

open System

let rec private collectWhileRec (transform: 'a -> Result<'b, 'c>) (sequence: seq<'a>) (accumulator: List<'b>) =
    if Seq.isEmpty sequence then
        Ok(accumulator |> List.rev)
    else
        match transform (Seq.head sequence) with
        | Ok v -> collectWhileRec transform (Seq.tail sequence) (v :: accumulator)
        | Error e -> Error e

let collectWhile (transform: 'a -> Result<'b, 'c>) (sequence: seq<'a>): Result<List<'b>, 'c> =
    collectWhileRec transform sequence List.empty

let resultToOption (result: Result<'a, 'b>): 'a option =
    match result with
    | Ok v -> Some v
    | Error _ -> None

let stringToInt (string: string): int option =
    match Int32.TryParse string with
    | true, out -> Some out
    | false, _ -> None
