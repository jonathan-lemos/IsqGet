module IsqGet.Functional

open System
open System.Text.RegularExpressions

let private seqTryHeadSafe (sequence: seq<'a>): 'a option =
    try
        Seq.tryHead sequence
    with :? ArgumentException -> None

let rec private collectResultWhileRec (transform: 'a -> Result<'b, 'c>) (sequence: seq<'a>) (accumulator: List<'b>) =
    match seqTryHeadSafe sequence with
    | Some head ->
        match transform head with
        | Ok v -> collectResultWhileRec transform (Seq.tail sequence) (v :: accumulator)
        | Error e -> Error e
    | None -> Ok(accumulator |> List.rev)

let collectResultWhile (transform: 'a -> Result<'b, 'c>) (sequence: seq<'a>): Result<List<'b>, 'c> =
    collectResultWhileRec transform sequence List.empty

let rec private collectWhileRec (transform: 'a -> Option<'b>) (sequence: seq<'a>) (accumulator: List<'b>) =
    if Seq.isEmpty sequence then
        Some(accumulator |> List.rev)
    else
        match transform (Seq.head sequence) with
        | Some v -> collectWhileRec transform (Seq.tail sequence) (v :: accumulator)
        | None -> None

let collectWhile (transform: 'a -> Option<'b>) (sequence: seq<'a>): List<'b> option =
    collectWhileRec transform sequence List.empty


let resultToOption (result: Result<'a, 'b>): 'a option =
    match result with
    | Ok v -> Some v
    | Error _ -> None

let optionToResult (error: 'b) (option: 'a option): Result<'a, 'b> =
    match option with
    | Some s -> Ok s
    | None -> Error error

let stringToInt (string: string): int option =
    match Int32.TryParse string with
    | true, out -> Some out
    | false, _ -> None

let stringToIntResult (string: string): Result<int, string> =
    stringToInt string
    |> optionToResult (string + " is not an int.")

let stringToDouble (string: string): double option =
    match Double.TryParse string with
    | true, out -> Some out
    | false, _ -> None

let stringToDoubleResult (string: string): Result<double, string> =
    stringToDouble string
    |> optionToResult (string + " is not a double.")

let regexNthCapture (pattern: string) (number: int) (input: string): string option =
    let matches =
        (Regex.Match(input, pattern)).Groups.Values
        |> Seq.toArray

    if number >= matches.Length then None else Some matches.[number].Value

let regexCapture (pattern: string) (input: string): string option = regexNthCapture pattern 1 input

type OptionBuilder() =
    member _.Bind(option: 'a option, transform: 'a -> 'b option): 'b option =
        match option with
        | Some a -> transform a
        | None -> None

    member _.Return(value: 'a): 'a option = Some value

let option = OptionBuilder()

type ResultBuilder() =
    member _.Bind(result: Result<'a, 'b>, transform: 'a -> Result<'c, 'b>): Result<'c, 'b> =
        match result with
        | Ok a -> transform a
        | Error e -> Error e

    member _.Return(value: 'a): Result<'a, 'b> = Ok value

    member _.ReturnFrom(value: Result<'a, 'b>): Result<'a, 'b> = value

let result = ResultBuilder()

let asAsync (value: 'T): Async<'T> = async { return value }

let asResult (value: 'a): Result<'a, 'b> = Ok value

let asyncMap (transform: 'a -> 'b) (value: Async<'a>): Async<'b> =
    async {
        let! v = value
        return transform v
    }

type AsyncResultBuilder() =
    member _.Bind(resultAsync: Async<Result<'a, 'b>>, transform: 'a -> Async<Result<'c, 'b>>): Async<Result<'c, 'b>> =
        async {
            let! result = resultAsync

            return!
                match result with
                | Ok a -> transform a
                | Error e -> Error e |> asAsync
        }

    member _.Return(value: 'a): Async<Result<'a, 'b>> = Ok value |> asAsync

    member _.ReturnFrom(value: Async<Result<'a, 'b>>): Async<Result<'a, 'b>> = value

let asyncResult = AsyncResultBuilder()

let rec retryWithDelayAsync (fn: unit -> Async<Result<'a, 'b>>)
                            (retryDelayMs: int)
                            (retryCount: int)
                            : Async<Result<'a, 'b>> =
    async {
        let! result = fn ()

        return!
            match (retryCount, result) with
            | (count, result) when count <= 0 -> result |> asAsync
            | (_count, Ok value) -> Ok value |> asAsync
            | (_count, Error _) -> retryWithDelayAsync fn retryDelayMs (retryCount - 1)
    }

let seqDeconstruct (sequence: seq<'T>): 'T * seq<'T> = (Seq.head sequence, Seq.tail sequence)
