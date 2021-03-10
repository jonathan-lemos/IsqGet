namespace IsqGet

open IsqGet

type Outputter = seq<string> -> Result<unit, string>

type Range(lower: int, upper: int) =
    member this.includes(year: int): bool = lower <= year && year <= upper

type Args(serializer: Serialize.Serializer, professors: List<string>, courses: List<string>, years: List<Range>, outputter: Outputter) =
    static member private outputToStdout (s: seq<string>) =
        for string in s do
            printf "%s" string
        Ok ()
    
    new() = Args(Serialize.Streaming Serialize.toMultilineJson, [], [], [], Args.outputToStdout)
    
    member this.serialize(entries: seq<Csv.Entry>): Choice<seq<string>, string> =
        match serializer with
        | Serialize.Streaming func -> Choice1Of2 (func entries)
        | Serialize.Greedy func -> Choice2Of2 (func entries)
        
    member this.output(segments: seq<string>) =
        outputter segments

    member this.matchesProfessor(professor: string) =
        if professors.IsEmpty then
            true
        else
            professors
            |> Seq.exists (Str.shouldMatch professor)

    member this.matchesCourse (courseCode: string) (courseName: string) =
        if courses.IsEmpty then
            true
        else
            courses
            |> Seq.exists (fun entry ->
                Str.shouldMatch courseCode entry
                || Str.shouldMatch courseName entry)

    member this.matchesYear(year: int) =
        if years.IsEmpty then
            true
        else
            years
            |> Seq.exists (fun range -> range.includes year)

    member this.withSerializer(newSerializer: Serialize.Serializer): Args =
        Args(newSerializer, professors, courses, years, outputter)

    member this.withProfessor(professor: string): Args =
        Args(serializer, professor :: professors, courses, years, outputter)

    member this.withCourse(course: string): Args =
        Args(serializer, professors, course :: courses, years, outputter)

    member this.withYears(range: Range): Args =
        Args(serializer, professors, courses, range :: years, outputter)
        
    member this.withOutputter(newOutputter: Outputter): Args =
        Args(serializer, professors, courses, years, newOutputter)

    static member printHelp(progName: string) =
        printfn "Usage: %s [...Args?]" progName
        printfn "Args:"
        printfn "\t--professor [professor]"
        printfn "\t\tInclude only the given professor. Can be given more than once to return multiple professors."
        printfn "\t--course [course]"

        printfn
            "\t\tInclude only the given course name/course code. Can be given more than once to return multiple courses."

        printfn "\t--year [1234 or 1234-1236]"
        printfn "\t\tInclude only the given year(s). Can be given more than once to return multiple years."

    static member private parseOptionWithArgument (cmdline: seq<string>)
                                                  (args: Args)
                                                  (ifNotEmpty: Args -> string -> seq<string> -> Result<seq<string> * Args, string>)
                                                  (emptyErrorMsg: string)
                                                  =
        if Seq.isEmpty cmdline then
            Error emptyErrorMsg
        else
            let (head, tail) = Functional.seqDeconstruct cmdline
            ifNotEmpty args head tail

    static member private parseProfessor (cmdline: seq<string>) (args: Args) =
        Args.parseOptionWithArgument cmdline args (fun args head tail -> Ok(tail, args.withProfessor head))
            "--professor requires a professor's name as an argument"

    static member private parseCourse (cmdline: seq<string>) (args: Args) =
        Args.parseOptionWithArgument cmdline args (fun args head tail -> Ok(tail, args.withCourse head))
            "--course requires a course's name as an argument"

    static member private parseYear (cmdline: seq<string>) (args: Args) =
        Args.parseOptionWithArgument cmdline args (fun args head tail ->
            match head with
            | Str.ParseRegex "^(\d+)-(\d+)$" [ n1; n2 ] -> Ok(tail, args.withYears (Range(int n1, int n2)))
            | Str.ParseRegex "^(\d+)$" [ n ] -> Ok(tail, args.withYears (Range(int n, int n)))
            | _ -> Error(sprintf "--year argument '%s' is malformed. Must be of the format '1234' or '2010-2015'" head))
            "--year requires a year (1234) or years (2010-2015) as an argument."

    static member private parseRec (cmdline: seq<string>) (args: Args): Result<Args, string> =
        if Seq.isEmpty cmdline then
            Ok args
        else
            let head = Seq.head cmdline

            let result =
                match head with
                | "--professor" -> Args.parseProfessor cmdline args
                | "--course" -> Args.parseCourse cmdline args
                | "--year" -> Args.parseYear cmdline args
                | _ -> Error (sprintf "Unrecognized option '%s'" head)

            match result with
            | Ok (tail, args) -> Args.parseRec tail args
            | Error e -> Error e


    static member parse(cmdline: seq<string>): Result<Args, string> = Args.parseRec cmdline (Args())
