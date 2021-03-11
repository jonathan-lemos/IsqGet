// Learn more about F# at http://fsharp.org

open System
open FSharp.Control
open IsqGet

let get url =
    Functional.retryWithDelayAsync (fun () -> Http.get url) 2000 3

let post url body contentType =
    Functional.retryWithDelayAsync (fun () -> Http.post url body contentType) 2000 3

let randInt (lower: int) (upper: int) = (Random()).Next(lower, upper)

let getTerms () =
    Functional.asyncResult {
        let! isqHtml = Fetch.getIsqHtml get

        let! isqDoc =
            Fetch.htmlToDocument isqHtml
            |> Functional.asyncMap Functional.asResult

        let! terms =
            Fetch.termsFromIsqDocument isqDoc
            |> Functional.asAsync

        return terms
    }

let getDepartmentsFromTerm (term: Term) =
    Functional.asyncResult {
        let! html = Fetch.getIsqHtmlWithTerm post term

        let! doc =
            Fetch.htmlToDocument html
            |> Functional.asyncMap Functional.asResult

        let! depts =
            Fetch.departmentsFromIsqTermDocument doc
            |> Functional.asAsync

        return depts
    }

let getEntriesFromTermAndDepartment (term: Term) (department: Department) =
    Functional.asyncResult {
        let! csv = Fetch.getIsqCsv get term department

        let! entries =
            Csv.parseCsv term department csv
            |> Functional.asAsync

        return entries
    }

let processTermDepts (term: Term) (departments: seq<Department>) =
    seq {
        for department in departments do
            yield
                getEntriesFromTermAndDepartment term department
                |> Async.RunSynchronously
    }

let processTerms (terms: seq<Term>) =
    seq {
        for term in terms do
            match getDepartmentsFromTerm term
                  |> Async.RunSynchronously
                  |> Result.map (processTermDepts term) with
            | Ok s -> yield! s
            | Error e -> yield Error e
    }

let processEntrySequence (outputError: string -> unit) (sequence: seq<Result<List<Csv.Entry>, string>>) =
    seq {
        for entry in sequence do
            match entry with
            | Ok entries -> yield! entries
            | Error e -> outputError e
    }


let printError (s: string) = eprintfn "\027[31;1m[ERROR] %s\027[m" s

let getEntrySequence () =
    Functional.asyncResult {
        let! terms = getTerms ()
        let resultSequence = processTerms terms

        let entrySequence =
            processEntrySequence printError resultSequence

        return entrySequence
    }

let getOutputSequence (args: Args) =
    Functional.asyncResult {
        let! result = getEntrySequence ()
        return args.serialize result
    }

let withArgs (args: Args) =
    async {
        let! result = getOutputSequence args
        return result |> Result.bind args.output
    }

[<EntryPoint>]
let main argv =
    let result =
        Functional.result {
            let! args = Args.parse argv
            return! withArgs args |> Async.RunSynchronously
        }

    match result with
    | Ok () -> 0
    | Error e ->
        printError e
        1
