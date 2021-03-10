module IsqGet.Str

open System.Text.RegularExpressions

let shouldMatch (haystack: string) (needle: string): bool = haystack.ToLower().Contains needle

let (|ParseRegex|_|) pattern input =
    let m = Regex.Match(pattern, input)

    if m.Success
    then Some(List.tail [ for x in m.Groups -> x.Value ])
    else None
