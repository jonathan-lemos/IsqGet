namespace IsqGet

open System.Collections.Generic
open IsqGet

type Outputter = seq<string> -> Result<unit, string>

type Range(lower: int, upper: int) =
    member this.includes(year: int): bool = lower <= year && year <= upper

type Args(serializer: Serialize.Serializer,
          professors: string list,
          courses: string list,
          years: Range list,
          outputter: Outputter) =
    static member private outputToStdout(s: seq<string>) =
        for string in s do
            printf "%s" string

        Ok()

    new() = Args(Serialize.toJson, [], [], [], Args.outputToStdout)

    member this.serialize = serializer

    member this.output(segments: seq<string>) = outputter segments

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

    static member private parseOptionWithArgument (cmdline: IEnumerator<string>)
                                                  (args: Args)
                                                  (ifNotEmpty: Args -> string -> IEnumerator<string> -> Result<IEnumerator<string> * Args, string>)
                                                  (emptyErrorMsg: string)
                                                  =
        match Functional.enumeratorDeconstruct cmdline with
        | Some (head, tail) -> ifNotEmpty args head tail
        | None -> Error emptyErrorMsg

    static member private parseProfessor (cmdline: IEnumerator<string>) (args: Args) =
        Args.parseOptionWithArgument cmdline args (fun args head tail -> Ok(tail, args.withProfessor head))
            "--professor requires a professor's name as an argument"

    static member private parseCourse (cmdline: IEnumerator<string>) (args: Args) =
        Args.parseOptionWithArgument cmdline args (fun args head tail -> Ok(tail, args.withCourse head))
            "--course requires a course's name as an argument"

    static member private parseYear (cmdline: IEnumerator<string>) (args: Args) =
        Args.parseOptionWithArgument cmdline args (fun args head tail ->
            match head with
            | Str.ParseRegex "^(\d+)-(\d+)$" [ n1; n2 ] -> Ok(tail, args.withYears (Range(int n1, int n2)))
            | Str.ParseRegex "^(\d+)$" [ n ] -> Ok(tail, args.withYears (Range(int n, int n)))
            | _ -> Error(sprintf "--year argument '%s' is malformed. Must be of the format '1234' or '2010-2015'" head))
            "--year requires a year (1234) or years (2010-2015) as an argument."

    static member private parseRec (cmdline: IEnumerator<string>) (args: Args): Result<Args, string> =
        match Functional.enumeratorDeconstruct cmdline with
        | Some (head, tail) ->
            let result =
                Functional.result {
                    let! parser =
                        match head with
                        | "--professor" -> Ok Args.parseProfessor
                        | "--course" -> Ok Args.parseCourse
                        | "--year" -> Ok Args.parseYear
                        | _ -> Error(sprintf "Unrecognized option '%s'" head)

                    return! parser tail args
                }

            match result with
            | Ok (tail, args) -> Args.parseRec tail args
            | Error e -> Error e
        | None -> Ok args


    static member parse(cmdline: seq<string>): Result<Args, string> = Args.parseRec (cmdline.GetEnumerator()) (Args())
