module IsqGet.Csv

open System.Globalization
open System.IO
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
            | Str.ParseRegexFrom "\\G\"(?=\\s*(,|$))" index _matches -> index
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
            | Str.ParseRegexFrom "\\G\s*$" index _matches -> Ok(entries |> List.rev)
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

let parseCsv (term: Term) (department: Department) (csv: string): Result<Entry, string> seq =
    let lines =
        csv.Split("\n")
        |> Iterator
        |> Iterator.tail
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> x <> "")
        |> Seq.toList

    if lines |> List.isEmpty then
        Seq.empty
    else
        lines
        |> csvToFields
        |> Seq.map (fun fieldsResult ->
            fieldsResult
            |> Result.bind (fun fields ->
                if fields.Length < 20 then
                    Error
                        (sprintf
                            "Line '%s' is invalid: expected at least 20 fields, got %d."
                             (fields |> String.concat ",")
                             fields.Length)
                else
                    Functional.result {
                        let courseCode = fields.[0]
                        let courseName = fields.[1]
                        let professorName = fields.[3]

                        let gpa = fields.[16] |> Functional.stringToDouble

                        let! enrolled = fields.[17] |> Functional.stringToIntResult

                        let responseRateField =
                            fields.[18].Trim()
                            |> Functional.regexCapture "^(.*)%$"

                        let responseRate =
                            match responseRateField with
                            | Some value ->
                                value
                                |> Functional.stringToDouble
                                |> Option.map (fun number -> number * 0.01)
                            | None -> None

                        let rating =
                            fields.Last() |> Functional.stringToDouble

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
                    }))
