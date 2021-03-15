module IsqGet.Tests.Csv

open IsqGet.Csv
open NUnit.Framework

[<Test>]
let ``Parses sample csv fields properly`` () =
    let sample = """
"quoted", not quoted,"embedded "quote""   
"spaces at end  ",""full quote"",normal
    """
    let sample = sample.Trim()
    
    let fields = sample.Split("\n") |> Array.toList |> csvToFields
    let expected: Result<string list, string> list = [
        Ok ["quoted"; "not quoted"; "embedded \"quote\""]
        Ok ["spaces at end  "; "\"full quote\""; "normal"]
    ]
    
    Assert.AreEqual(fields, expected)
    
    