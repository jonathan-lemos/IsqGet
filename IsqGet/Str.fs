module IsqGet.Str

open System.Text.RegularExpressions

let regexNthCapture (pattern: string) (number: int) (input: string): string option =
    let matches =
        (Regex.Match(input, pattern)).Groups.Values
        |> Seq.toArray

    if number >= matches.Length then None else Some matches.[number].Value

let regexCapture (pattern: string) (input: string): string option = regexNthCapture pattern 1 input

let shouldMatch (haystack: string) (needle: string): bool = haystack.ToLower().Contains (needle.ToLower())

let parseRegexFrom (pattern: string) (fromIndex: int) (input: string): string list option =
    if fromIndex >= input.Length then
        None
    else
        let m = Regex(pattern).Match(input, fromIndex)

        if m.Success
        then Some(List.tail [ for x in m.Groups -> x.Value ])
        else None

let parseRegex (pattern: string) (input: string): string list option =
    parseRegexFrom pattern 0 input
    

let (|ParseRegex|_|) (pattern: string) (input: string) =
    parseRegex pattern input

let (|ParseRegexFrom|_|) (pattern: string) (fromIndex: int) (input: string) =
    parseRegexFrom pattern fromIndex input
    