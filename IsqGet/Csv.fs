module IsqGet.Csv

open System.Globalization
open System.IO
open System.Linq
open CsvHelper

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

let rec private readerSequence (reader: CsvReader): seq<CsvReader> =
    if reader.Read() then
        seq {
            yield reader
            yield! (readerSequence reader)
        }
    else
        Seq.empty

let parseCsv (term: Term) (department: Department) (csv: string): Result<List<Entry>, string> =
    use textReader = new StringReader(csv)

    use reader =
        new CsvReader(textReader, CultureInfo.InvariantCulture)

    if not (reader.Read() && reader.ReadHeader()) then
        Error "Invalid CSV"
    else
        readerSequence reader
        |> Functional.collectResultWhile (fun currentReader ->
            let fields =
                Enumerable.Range(0, currentReader.HeaderRecord.Length)
                |> Seq.map currentReader.GetField
                |> Seq.toArray

            if fields.Length < 20 then
                Error(sprintf "Line '%s' is invalid: expected at least 20 fields, got %d." (fields |> String.concat ",") fields.Length)
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

                    let rating = fields.Last() |> Functional.stringToDouble

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
                })
