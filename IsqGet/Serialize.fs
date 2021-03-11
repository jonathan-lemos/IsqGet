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
    match entry.gpa with
    | Some value -> writer.WriteValue value
    | None -> writer.WriteNull()
    
    writer.WritePropertyName "professorName"
    writer.WriteValue entry.professorName
    
    writer.WritePropertyName "rating"
    writer.WriteValue entry.rating
    
    writer.WritePropertyName "responseRate"
    writer.WriteValue entry.responseRate
    
    writer.WritePropertyName "term"
    writer.WriteStartObject()
    
    writer.WritePropertyName "id"
    writer.WriteValue entry.term.id
    
    writer.WritePropertyName "season"
    writer.WriteValue (Term.seasonToString entry.term.season)

    writer.WritePropertyName "year"
    writer.WriteValue entry.term.year
    
    writer.WriteEndObject()
    
    writer.WriteEndObject()
    
    sb.ToString()

let rec private loopAndYieldJson (enumerator: IEnumerator<Csv.Entry>) (leadingComma: bool) =
    seq {
        if leadingComma then
            yield ","

        yield!
            match Functional.enumeratorDeconstruct enumerator with
            | Some (head, tail) ->
                seq {
                    yield serializeEntry head
                    yield! loopAndYieldJson tail true
                }
            | None -> Seq.empty
    }

let toJson (entries: seq<Csv.Entry>): seq<string> =
    seq {
        yield "["
        yield! loopAndYieldJson (entries.GetEnumerator()) false
        yield "]"
    }
