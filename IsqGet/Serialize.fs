module IsqGet.Serialize

open System.Collections.Generic
open System.IO
open System.Text
open Newtonsoft.Json

type Serializer = seq<Csv.Entry> -> seq<string>

let serializeEntry (entry: Csv.Entry) =
    let sb = StringBuilder()
    use sw = new StringWriter(sb)
    use writer = new JsonTextWriter(sw)

    let writeDoubleOption (value: double option) =
        match value with
        | Some v -> writer.WriteValue v
        | None -> writer.WriteNull()

    writer.WriteStartObject()

    writer.WritePropertyName "courseCode"
    writer.WriteValue entry.courseCode

    writer.WritePropertyName "courseName"
    writer.WriteValue entry.courseName

    writer.WritePropertyName "department"
    writer.WriteStartObject()

    writer.WritePropertyName "id"
    writer.WriteValue entry.department.id

    writer.WritePropertyName "name"
    writer.WriteValue entry.department.name

    writer.WriteEndObject()

    writer.WritePropertyName "enrolled"
    writer.WriteValue entry.enrolled

    writer.WritePropertyName "gpa"
    writeDoubleOption entry.gpa

    writer.WritePropertyName "professorName"
    writer.WriteValue entry.professorName

    writer.WritePropertyName "rating"
    writeDoubleOption entry.rating

    writer.WritePropertyName "responseRate"
    writeDoubleOption entry.responseRate

    writer.WritePropertyName "term"
    writer.WriteStartObject()

    writer.WritePropertyName "id"
    writer.WriteValue entry.term.id

    writer.WritePropertyName "season"
    writer.WriteValue(Term.seasonToString entry.term.season)

    writer.WritePropertyName "year"
    writer.WriteValue entry.term.year

    writer.WriteEndObject()

    writer.WriteEndObject()

    sb.ToString()

let rec private loopAndYieldJson (enumerator: IEnumerator<Csv.Entry>) =
    seq {
        yield!
            match Functional.enumeratorDeconstruct enumerator with
            | Some (head, tail) ->
                seq {
                    yield serializeEntry head
                    yield! loopAndYieldJson tail
                }
            | None -> Seq.empty
    }

let rec private loopAndYieldJsonWithComma (enumerator: IEnumerator<Csv.Entry>) =
    seq {
        for entry in loopAndYieldJson enumerator do
            yield ","
            yield entry
    }

let toJson (entries: seq<Csv.Entry>): seq<string> =
    seq {
        yield "["
        yield! loopAndYieldJson (entries.GetEnumerator())
        yield "]"
    }
