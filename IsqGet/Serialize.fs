module IsqGet.Serialize

open System.IO
open System.Text
open IsqGet.Functional
open Newtonsoft.Json
open IsqGet.Functional.Iterator
open IsqGet

type Serializer = seq<Csv.Entry> -> seq<string>

let serializeEntry (entry: Csv.Entry) =
    let sb = StringBuilder()
    use sw = new StringWriter(sb)
    use writer = new JsonTextWriter(sw)

    let rec writeKeyValue (key: string) (value: #obj) =
        writer.WritePropertyName key
        writeValue value

    and writeValue (value: obj) =
        match value with
        | :? string as s -> writer.WriteValue s
        | :? int as i -> writer.WriteValue i
        | :? double as d -> writer.WriteValue d
        | :? Term as t -> writeTerm t
        | :? Department as dept -> writeDepartment dept
        | :? (Option<double>) as o ->
            match o with
            | Some s -> writeValue s
            | None -> writer.WriteNull()
        | otherwise -> writeValue (sprintf "%A" otherwise)

    and writeTerm (term: Term) =
        writeObject([
            ("id", term.id :> obj)
            ("season", term.season :> obj)
            ("year", term.year :> obj)
        ])

    and writeDepartment (dept: Department) =
        writeObject([
            ("id", dept.id :> obj)
            ("name", dept.name :> obj)
        ])

    and writeObject (pairs: (string * obj) seq) =
        writer.WriteStartObject()

        pairs
        |> Seq.iter (fun (key, value) -> writeKeyValue key value)

        writer.WriteEndObject()

    writeObject([
        ("courseCode", entry.courseCode :> obj)
        ("courseName", entry.courseName :> obj)
        ("department", entry.department :> obj)
        ("enrolled", entry.enrolled :> obj)
        ("gpa", entry.gpa :> obj)
        ("professorName", entry.professorName :> obj)
        ("rating", entry.rating :> obj)
        ("responseRate", entry.responseRate :> obj)
        ("term", entry.term :> obj)
    ])

    sb.ToString()

let rec private loopAndYieldJsonEntries (iterator: Iterator<Csv.Entry>) =
    match iterator with
    | Deconstruct (head, tail) ->
        seq {
            yield serializeEntry head
            yield! loopAndYieldJsonEntries tail
        }
    | Empty -> Seq.empty

let toJson (entries: Csv.Entry seq): string seq =
    let entryIterator =
        loopAndYieldJsonEntries (entries |> Iterator)
        |> Iterator

    let rec generateCommaSeparatedSequence (jsonStringIterator: Iterator<string>) =
        match jsonStringIterator with
        | Deconstruct (head, tail) ->
            seq {
                yield ","
                yield head
                yield! generateCommaSeparatedSequence tail
            }
        | Empty -> Seq.empty

    match entryIterator with
    | Empty -> [ "[]" ] |> List.toSeq
    | Deconstruct (head, tail) ->
        seq {
            yield "["
            yield head
            yield! generateCommaSeparatedSequence tail
            yield "]"
        }
