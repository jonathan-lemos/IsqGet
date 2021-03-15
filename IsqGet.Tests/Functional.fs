module IsqGet.Tests.Functional

open IsqGet.Functional.Transform
open NUnit.Framework

[<Test>]
let ``mapWhile works on all Ok mappings`` () =
    let result =
        seq { 1 .. 3 } |> mapWhile (fun i -> Ok(i + 1))

    match result with
    | Ok [ 2; 3; 4 ] -> Assert.Pass()
    | Ok _ -> Assert.Fail("Expected 1-2-3 to become 2-3-4")
    | Error _e -> Assert.Fail("Expected Ok result")

[<Test>]
let ``mapWhile works on Error mapping`` () =
    let result =
        seq { 1 .. 5 }
        |> mapWhile (fun i ->
            match i with
            | 2 -> Error "foo bar"
            | i -> Ok(i + 1))

    match result with
    | Ok _result -> Assert.Fail("Expected Error result")
    | Error "foo bar" -> Assert.Pass()
    | Error _e -> Assert.Fail("Expected 'foo bar' error")
