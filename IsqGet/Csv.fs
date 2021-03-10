module IsqGet.Csv

open System.Globalization
open System.IO
open System.Linq
open CsvHelper

type Entry =
    { courseCode: string
      courseName: string
      professorName: string
      gpa: double
      enrolled: int
      responseRate: double
      rating: double }

let rec private readerSequence (reader: CsvReader) =
    if reader.Read() then
        seq {
            yield reader
            yield! readerSequence reader
        }
    else
        Seq.empty

let parseCsv (csv: string): Result<List<Entry>, string> =
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

            if fields.Length <> 20 then
                Error(sprintf "Line '%s' is invalid: expected 20 fields." (fields |> String.concat ","))
            else
                Functional.result {
                    let courseCode = fields.[0]
                    let courseName = fields.[1]
                    let professorName = fields.[3]

                    let! gpa = fields.[16] |> Functional.stringToDoubleResult

                    let! enrolled = fields.[17] |> Functional.stringToIntResult

                    let! responseRateField =
                        fields.[18].Trim()
                        |> Functional.regexCapture "^(.*)%$"
                        |> Functional.optionToResult ("'" + fields.[18] + "' is malformed.")

                    let! responseRate =
                        responseRateField
                        |> Functional.stringToDoubleResult
                        |> Result.map (fun number -> number * 0.01)

                    let! rating = fields.[19] |> Functional.stringToDoubleResult

                    return
                        { courseCode = courseCode
                          courseName = courseName
                          professorName = professorName
                          gpa = gpa
                          enrolled = enrolled
                          responseRate = responseRate
                          rating = rating }
                })
