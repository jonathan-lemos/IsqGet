module IsqGet.Tests.Iterator

open IsqGet.Functional
open NUnit.Framework
open IsqGet.Functional.Iterator

[<SetUp>]
let Setup () = ()

[<Test>]
let ``Should inherit methods from Seq`` () =
    let iterator = seq { 1 .. 5 } |> Iterator
    Assert.AreEqual(seq { 1 .. 5 } |> Seq.toList, iterator |> Seq.toList)

[<Test>]
let ``Should deconstruct into head and tail`` () =
    let iterator = seq { 1 .. 5 } |> Iterator

    match iterator |> Iterator.deconstruct with
    | Some (head, tail) ->
        Assert.AreEqual(head, 1)
        Assert.AreEqual(tail |> Seq.toList, seq { 2 .. 5 } |> Seq.toList)
    | None -> Assert.Fail("Expected iterator deconstruct to return (head, tail)")
    
[<Test>]
let ``Should deconstruct using pattern match`` () =
    let iterator = seq { 1 .. 5 } |> Iterator

    match iterator with
    | Deconstruct (head, tail) ->
        Assert.AreEqual(head, 1)
        Assert.AreEqual(tail |> Seq.toList, seq { 2 .. 5 } |> Seq.toList)
    | Empty -> Assert.Fail("Expected iterator deconstruct to return (head, tail)")

[<Test>]
let ``Should deconstruct empty on empty iterator`` () =
    let iterator = Seq.empty |> Iterator

    match iterator with
    | Deconstruct (_head, _tail) -> Assert.Fail("Expected iterator to not deconstruct since it's empty.")
    | Empty -> Assert.Pass()
