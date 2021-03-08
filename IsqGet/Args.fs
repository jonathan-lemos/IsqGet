module IsqGet

open IsqGet

type Range(lower: int, upper: int) =
    member this.includes(year: int): bool = lower <= year && year <= upper

type Args(serializer: Serialize.Serializer, professors: List<string>, courses: List<string>, years: List<Range>) =
    member this.serializer = serializer
    
    new() = Args(Serialize.Streaming Serialize.toMultilineJson, [], [], [])

    member this.matchesProfessor(professor: string) =
        if professors.IsEmpty
        then true
        else professors |> Seq.exists (Str.shouldMatch professor)

    member this.matchesCourse (courseCode: string) (courseName: string) =
        if courses.IsEmpty then
            true
        else
            courses
            |> Seq.exists (fun entry ->
                Str.shouldMatch courseCode entry
                || Str.shouldMatch courseName entry)

    member this.matchesYear (year: int) =
        if years.IsEmpty then
            true
        else
            years |> Seq.exists (fun range -> range.includes year)
    
    member this.withSerializer (newSerializer: Serialize.Serializer): Args =
        Args(newSerializer, professors, courses, years)
        
    member this.withProfessor (professor: string): Args =
        Args(serializer, professor :: professors, courses, years)
        
    member this.withCourse (course: string): Args =
        Args(serializer, professors, course :: courses, years)
        
    member this.withYears (range: Range): Args =
        Args(serializer, professors, courses, range :: years)
        
    static member printHelp (progName: string) =
        printfn "Usage: %s [...Args?]" progName
        printfn "Args:"
        printfn "\t--professor [professor]"
        printfn "\t\tInclude only the given professor. Can be given more than once to return multiple professors."
        printfn "\t--course [course]"
        printfn "\t\tInclude only the given course name/course code. Can be given more than once to return multiple courses."
        printfn "\t--year [1234 or 1234-1236]"
        printfn "\t\tInclude only the given year(s). Can be given more than once to return multiple years."
        
    static member private parseOptionWithArgument (cmdline: seq<string>) (args: Args) (ifNotEmpty: Args -> string -> seq<string> -> Result<seq<string> * Args, string>) (emptyErrorMsg: string) =
        if Seq.isEmpty cmdline then
            Error emptyErrorMsg
        else
            let (head, tail) = Functional.seqDeconstruct cmdline
            ifNotEmpty args head tail
        
    static member private parseProfessor (cmdline: seq<string>) (args: Args) =
        Args.parseOptionWithArgument cmdline args (fun args head tail -> Ok (tail, args.withProfessor head)) "--professor requires a professor's name as an argument"
    
    static member private parseCourse (cmdline: seq<string>) (args: Args) =
        Args.parseOptionWithArgument cmdline args (fun args head tail -> Ok (tail, args.withCourse head)) "--course requires a course's name as an argument"
        
    static member private parseYear (cmdline: seq<string>) (args: Args) =
        Args.parseOptionWithArgument cmdline args (fun args head tail ->
            match head with
            
            )
        
    static member parseRec (cmdline: seq<string>) (args: Args) =
        if Seq.isEmpty cmdline then
            Ok args
        else  
            let (head, tail) = Functional.seqDeconstruct cmdline
            
            let result = match head with
                | "--professor" -> Args.parseProfessor cmdline args
                | _ -> Error "shit's fucked"
        
        
    static member parse (cmdline: Seq<string>): Args
        