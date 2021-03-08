module IsqGet.Serialize

open Newtonsoft.Json

type Serializer =
    | Streaming of (seq<Csv.Entry> -> seq<string>)
    | Greedy of (seq<Csv.Entry> -> string)

let rec toMultilineJson (entries: seq<Csv.Entry>): seq<string> =
    if Seq.isEmpty entries then
        Seq.empty
    else
        seq {
            yield Seq.head |> JsonConvert.SerializeObject
            yield! Seq.tail entries |> toMultilineJson
        }

let toJson (entries: seq<Csv.Entry>): string =
    entries
    |> Seq.toArray
    |> JsonConvert.SerializeObject

let toSqliteCommands (entries: seq<Csv.Entry>): seq<string> =
    seq {
        yield
            "
        CREATE TABLE Entries(
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            courseCode INTEGER NOT NULL,
            courseName TEXT NOT NULL,
            professorName TEXT NOT NULL,
            gpa REAL NOT NULL,
            enrolled INTEGER NOT NULL,
            responseRate REAL NOT NULL,
            rating REAL NOT NULL
        );
        "
                .Trim()

        yield!
            entries
            |> Seq.map (fun entry ->
                sprintf
                    "INSERT INTO Entries VALUES ('%s', '%s', '%s', %f, %d, %f, %f);"
                    entry.courseCode
                    entry.courseName
                    entry.professorName
                    entry.gpa
                    entry.enrolled
                    entry.responseRate
                    entry.rating)
    }
