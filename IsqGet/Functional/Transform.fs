module IsqGet.Functional.Transform

open IsqGet.Functional.Iterator
open IsqGet.Functional.Convert

let rec private mapWhileRec (transform: 'a -> Result<'b, 'c>) (iterator: Iterator<'a>) (accumulator: 'b list) =
    match iterator with
    | Empty -> Ok(accumulator |> List.rev)
    | Deconstruct (head, tail) ->
        match transform head with
        | Ok v -> mapWhileRec transform tail (v :: accumulator)
        | Error e -> Error e

let mapWhile (transform: 'a -> Result<'b, 'c>) (sequence: seq<'a>): Result<'b list, 'c> =
    mapWhileRec transform (sequence |> Iterator) List.empty

let rec private mapWhileIntoOptionRec (transform: 'a -> Option<'b>) (iterator: Iterator<'a>) (accumulator: 'b list) =
    match iterator with
    | Empty -> Some(accumulator |> List.rev)
    | Deconstruct (head, tail) ->
        match transform head with
        | Some v -> mapWhileIntoOptionRec transform tail (v :: accumulator)
        | None -> None

let mapWhileIntoOption (transform: 'a -> Option<'b>) (sequence: seq<'a>): 'b list option =
    mapWhileIntoOptionRec transform (sequence |> Iterator) List.empty
    
let asyncMap (transform: 'a -> 'b) (value: Async<'a>): Async<'b> =
    async {
        let! v = value
        return transform v
    }

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
