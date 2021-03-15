module IsqGet.Csv

open IsqGet.Functional
open IsqGet.Functional.Builders
open IsqGet.Functional.Convert
open IsqGet.Str
open System.Linq

type Entry =
    { term: Term
      department: Department
      courseCode: string
      courseName: string
      professorName: string
      gpa: double Option
      enrolled: int
      responseRate: double Option
      rating: double Option }

type private CsvFieldOpenType =
    | Quoted of int
    | Unquoted of int
    | EndOfLine

let private parseCsvField (line: string) (index: int): (string * int) option =
    let rec findStart (index: int): CsvFieldOpenType =
        if index >= line.Length then
            EndOfLine
        else
            match line.[index] with
            | '"' -> Quoted(index + 1)
            | ' '
            | '\t' -> findStart (index + 1)
            | _ -> Unquoted index

    let rec findClosingQuoteOrEol (index: int): int =
        if index >= line.Length then
            line.Length
        else
            match line with
            | ParseRegexFrom "\\G\"(?=\\s*(,|$))" index _matches -> index
            | _ -> findClosingQuoteOrEol (index + 1)

    let rec findCommaOrEol (index: int): int =
        if index >= line.Length then
            line.Length
        else
            match line.[index] with
            | ',' -> index
            | _ -> findCommaOrEol (index + 1)

    let startingIndex = findStart index

    match startingIndex with
    | Quoted start ->
        let endingIndex = findClosingQuoteOrEol start
        let slice = line.[start..endingIndex - 1]
        // + 1 to seek past ending quote
        Some(slice, endingIndex + 1)
    | Unquoted start ->
        let endingIndex = findCommaOrEol start
        let slice = line.[start..endingIndex - 1]
        Some(slice, endingIndex)
    | EndOfLine -> None

let rec private parseCsvLineRec (line: string) (index: int) (entries: string list): Result<string list, string> =
    let verifyParsedToEndOfLineResult () =
        if index >= line.Length then
            Ok(entries |> List.rev)
        else
            match line with
            | ParseRegexFrom "\\G\s*$" index _matches -> Ok(entries |> List.rev)
            | _ -> Error(sprintf "Failed to parse CSV line starting with '%s'" line.[index..])

    let rec seekToCommaOrEol (index: int) =
        if index >= line.Length then
            index
        else
            match line.[index] with
            | ',' -> index
            | _ -> seekToCommaOrEol (index + 1)

    let tailOption =
        parseCsvField line index
        |> Option.map (fun (field, index) -> (field, (seekToCommaOrEol index) + 1))

    match tailOption with
    | Some (field, newIndex) -> parseCsvLineRec line newIndex (field :: entries)
    | None -> verifyParsedToEndOfLineResult ()

let rec csvToFields (lines: string list): Result<string list, string> seq =
    match lines with
    | head :: tail ->
        seq {
            yield parseCsvLineRec head 0 []
            yield! csvToFields tail
        }
    | [] -> Seq.empty
    
let parseCsvLine (term: Term) (department: Department) (fields: string seq): Result<Entry, string> =
    let fields = Seq.toArray fields
    
    if fields.Length < 20 then
        Error
            (sprintf
                "Line '%s' is invalid: expected at least 20 fields, got %d."
                 (fields |> String.concat ",")
                 fields.Length)
    else
        result {
            let courseCode = fields.[0].Trim()
            let courseName = fields.[1].Trim()
            let professorName = fields.[3].Trim()

            let gpa = fields.[16] |> stringToDouble

            let! enrolled = fields.[17] |> stringToIntResult

            let responseRateField =
                fields.[18].Trim()
                |> regexCapture "^(.*)%$"

            let responseRate =
                match responseRateField with
                | Some value ->
                    value
                    |> stringToDouble
                    |> Option.map (fun number -> number * 0.01)
                | None -> None

            let rating =
                fields.Last() |> stringToDouble

            return
                { term = term
                  department = department
                  courseCode = courseCode
                  courseName = courseName
                  professorName = professorName
                  gpa = gpa
                  enrolled = enrolled
                  responseRate = responseRate
                  rating = rating }
        }

let parseCsv (term: Term) (department: Department) (csv: string): Result<Entry, string> seq =
    let lines =
        csv.Split("\n")
        |> Iterator
        |> Iterator.tail // to skip header line
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter ((<>) "")
        |> Seq.toList

    if lines |> List.isEmpty then
        Seq.empty
    else
        lines
        |> csvToFields
        |> Seq.map (Result.bind (parseCsvLine term department))
