// Learn more about F# at http://fsharp.org

open System
open FSharp.Control
open IsqGet
open Functional.Transform
open Functional.Builders
open Functional.Convert

let get url =
    retryWithDelayAsync (fun () -> Http.get url) 2000 3

let post url body contentType =
    retryWithDelayAsync (fun () -> Http.post url body contentType) 2000 3

let randInt (lower: int) (upper: int) = (Random()).Next(lower, upper)

let getTerms () =
    asyncResult {
        let! isqHtml = Fetch.getIsqHtml get

        let! isqDoc = Fetch.htmlToDocument isqHtml |> asyncMap asResult

        let! terms = Fetch.termsFromIsqDocument isqDoc |> asAsync

        return terms
    }

let getDepartmentsFromTerm (filterDepts: Department -> bool) (term: Term) =
    asyncResult {
        let! html = Fetch.getIsqHtmlWithTerm post term

        let! doc = Fetch.htmlToDocument html |> asyncMap asResult

        let! depts =
            Fetch.departmentsFromIsqTermDocument doc
            |> Result.map (Seq.filter filterDepts)
            |> asAsync

        return depts
    }

let termAndDeptToEntrySequence (term: Term) (department: Department) =
    let csvAsync =
        asyncResult {
            let! csv = Fetch.getIsqCsv get term department

            return Csv.parseCsv term department csv
        }

    async {
        let! csvEntryResult = csvAsync

        return
            match csvEntryResult with
            | Ok sequence -> sequence
            | Error e -> [ Error e ] |> List.toSeq
    }

let termAndDeptSeqToEntrySequence (term: Term) (departments: seq<Department>) =
    seq {
        for department in departments do
            yield!
                termAndDeptToEntrySequence term department
                |> Async.RunSynchronously
    }

let termSeqToEntrySequence (filterDepts: Department -> bool) (terms: seq<Term>) =
    seq {
        for term in terms do
            match getDepartmentsFromTerm filterDepts term
                  |> Async.RunSynchronously
                  |> Result.map (termAndDeptSeqToEntrySequence term) with
            | Ok s -> yield! s
            | Error e -> yield Error e
    }

let filterErrorsAndUndesiredResultsFromEntrySequence (args: Args)
                                                     (outputError: string -> unit)
                                                     (sequence: seq<Result<Csv.Entry, string>>)
                                                     =
    seq {
        for result in sequence do
            match result with
            | Ok entry ->
                if args.matchesCourse entry.courseCode entry.courseName
                   && args.matchesDepartment entry.department
                   && args.matchesProfessor entry.professorName then
                    yield entry
            | Error e -> outputError e
    }


let printError (s: string) = eprintfn "\027[31;1m[ERROR] %s\027[m" s

let getEntrySequence (args: Args) =
    asyncResult {
        let! terms = getTerms ()
        let terms = terms |> Seq.filter args.matchesTerm

        let resultSequence =
            termSeqToEntrySequence args.matchesDepartment terms

        let entrySequence =
            filterErrorsAndUndesiredResultsFromEntrySequence args printError resultSequence

        return entrySequence
    }

let getOutputSequence (args: Args) =
    asyncResult {
        let! result = getEntrySequence args
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
        result {
            let! args = Args.parse argv
            return! withArgs args |> Async.RunSynchronously
        }

    match result with
    | Ok () -> 0
    | Error e ->
        printError e
        1
