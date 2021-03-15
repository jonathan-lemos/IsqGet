module IsqGet.Functional.Convert

open System

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
    
let asAsync (value: 'T): Async<'T> = async { return value }

let asResult (value: 'a): Result<'a, 'b> = Ok value
