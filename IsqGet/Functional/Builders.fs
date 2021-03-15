module IsqGet.Functional.Builders

open IsqGet.Functional.Convert

type OptionBuilder() =
    member _.Bind(option: 'a option, transform: 'a -> 'b option): 'b option =
        match option with
        | Some a -> transform a
        | None -> None

    member _.Return(value: 'a): 'a option = Some value

    member _.ReturnFrom(value: 'a option): 'a option = value

let option = OptionBuilder()

type ResultBuilder() =
    member _.Bind(result: Result<'a, 'b>, transform: 'a -> Result<'c, 'b>): Result<'c, 'b> =
        match result with
        | Ok a -> transform a
        | Error e -> Error e

    member _.Return(value: 'a): Result<'a, 'b> = Ok value

    member _.ReturnFrom(value: Result<'a, 'b>): Result<'a, 'b> = value

let result = ResultBuilder()

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
