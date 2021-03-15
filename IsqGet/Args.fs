namespace IsqGet

open IsqGet
open IsqGet.Functional
open IsqGet.Functional.Iterator
open IsqGet.Functional.Builders

type Outputter = seq<string> -> Result<unit, string>

type private TermSegmentParseResult =
    | Year of int
    | TermValue of Term
    | Invalid

type Range(lower: Term, upper: Term) =
    let (lower, upper) =
        if lower > upper then (upper, lower) else (lower, upper)

    member this.includes(term: Term): bool = lower <= term && term <= upper

    static member private parseSegment(segment: string): TermSegmentParseResult =
        match segment.ToLower() with
        | Str.ParseRegex "^\\d+$" _matches -> Year(int segment)
        | Str.ParseRegex "^([a-z]+)(\\d+)$" [ season; year ] ->
            let seasonValue =
                match season with
                | "spring" -> Some Spring
                | "summer" -> Some Summer
                | "fall" -> Some Fall
                | "s" -> Some Spring
                | "su" -> Some Summer
                | "f" -> Some Fall
                | _ -> None

            match seasonValue with
            | Some season -> TermValue(Term(season, int year))
            | None -> Invalid
        | _ -> Invalid

    static member private parseRange (segmentBegin: string) (segmentEnd: string): Range option =
        let tuple =
            match (Range.parseSegment segmentBegin, Range.parseSegment segmentEnd) with
            | (Year first, Year last) ->
                let (first, last) =
                    if first > last then (last, first) else (first, last)

                Some(Term(Spring, first), Term(Fall, last))
            | (TermValue first, TermValue last) -> Some(if first > last then (last, first) else (first, last))
            | _ -> None

        tuple |> Option.map Range

    static member private parseSingle(segment: string): Range option =
        let tuple =
            match Range.parseSegment segment with
            | Year year -> Some(Term(Spring, year), Term(Fall, year))
            | TermValue term -> Some(term, term)
            | Invalid -> None

        tuple |> Option.map Range

    static member fromString(string: string): Range option =
        match string with
        | Str.ParseRegex "^(.+)-(.+)$" [ first; second ] -> Range.parseRange first second
        | _ -> Range.parseSingle string




type Args =
    { serializer: Serialize.Serializer
      professors: string list
      courses: string list
      termRanges: Range list
      departments: string list
      outputter: Outputter }
    
    
    static member stdoutSerialize (s: string seq) =
        for string in s do
            printf "%s" string
        Ok()
     
    static member Default = {serializer = Serialize.toJson; professors = []; courses = []; termRanges = []; departments = []; outputter = Args.stdoutSerialize}

    member this.serialize = this.serializer

    member this.output(segments: string seq) = this.outputter segments

    member this.matchesProfessor(professor: string) =
        if this.professors.IsEmpty then
            true
        else
            this.professors
            |> Seq.exists (Str.shouldMatch professor)

    member this.matchesCourse (courseCode: string) (courseName: string) =
        if this.courses.IsEmpty then
            true
        else
            this.courses
            |> Seq.exists (fun entry ->
                Str.shouldMatch courseCode entry
                || Str.shouldMatch courseName entry)

    member this.matchesTerm(term: Term) =
        if this.termRanges.IsEmpty then
            true
        else
            this.termRanges
            |> Seq.exists (fun range -> range.includes term)
            
    member this.matchesDepartment(department: Department) =
        if this.departments.IsEmpty then
            true
        else
            this.departments
            |> Seq.exists (Str.shouldMatch department.name)

    member this.withSerializer(newSerializer: Serialize.Serializer): Args =
        { this with serializer = newSerializer }

    member this.withProfessor(professor: string): Args =
        { this with professors = professor :: this.professors }

    member this.withCourse(course: string): Args =
        { this with courses = course :: this.courses }

    member this.withTermRange(range: Range): Args =
        { this with termRanges = range :: this.termRanges }
        
    member this.withDepartment(department: string) =
        { this with departments = department :: this.departments }
        
    member this.withOutputter(newOutputter: Outputter): Args =
        { this with outputter = newOutputter }
           
    static member printHelp() =
        printfn "IsqGet %s" "0.0.1"
        printfn "Usage: %s [...Args?]" (System.Diagnostics.Process.GetCurrentProcess().ProcessName)
        printfn "Args:"
        printfn "\t--professor [professor]"
        printfn "\t\tInclude only the given professor. Can be given more than once to return multiple professors."
        printfn "\t--course [course]"

        printfn
            "\t\tInclude only the given course name/course code. Can be given more than once to return multiple courses."

        printfn "\t--term [2012 or Spring2012 or 2012-2015 or Spring2012-Fall2015]"
        printfn "\t\tInclude only the given term(s). Can be given more than once to return multiple terms."
        
        printfn "\t--department [department]"
        printfn "\t\tInclude only the given department. Can be given more than once to return multiple departments."

    static member private parseOptionWithArgument (cmdline: Iterator<string>)
                                                  (args: Args)
                                                  (ifNotEmpty: Args -> string -> Iterator<string> -> Result<Iterator<string> * Args, string>)
                                                  (emptyErrorMsg: string)
                                                  =
        match cmdline with
        | Deconstruct (head, tail) -> ifNotEmpty args head tail
        | Empty -> Error emptyErrorMsg

    static member private parseProfessor (cmdline: Iterator<string>) (args: Args) =
        Args.parseOptionWithArgument cmdline args (fun args head tail -> Ok(tail, args.withProfessor head))
            "--professor requires a professor's name as an argument"

    static member private parseCourse (cmdline: Iterator<string>) (args: Args) =
        Args.parseOptionWithArgument cmdline args (fun args head tail -> Ok(tail, args.withCourse head))
            "--course requires a course's name as an argument"
            
    static member private parseDepartment (cmdline: Iterator<string>) (args: Args) =
        Args.parseOptionWithArgument cmdline args (fun args head tail -> Ok(tail, args.withDepartment head))
            "--department requires a department name as an argument"

    static member private parseTermRange (cmdline: Iterator<string>) (args: Args) =
        Args.parseOptionWithArgument cmdline args (fun args head tail ->
            match Range.fromString head with
            | Some range -> Ok(tail, args.withTermRange range)
            | None -> Error(sprintf "--term argument '%s' is malformed. Must be of the format '2015-2020' or '2015' or 'Summer2014-Fall2018'" head))
            "--term requires a year (1234) or years (2010-2015) as an argument."

    static member private parseRec (cmdline: Iterator<string>) (args: Args): Result<Args, string> =
        match cmdline with
        | Deconstruct (head, tail) ->
            let result =
                result {
                    let! parser =
                        match head with
                        | "--professor" -> Ok Args.parseProfessor
                        | "--department" -> Ok Args.parseDepartment
                        | "--course" -> Ok Args.parseCourse
                        | "--term" -> Ok Args.parseTermRange
                        | "--help" ->
                            Args.printHelp()
                            exit 0
                        | _ -> Error(sprintf "Unrecognized option '%s'" head)

                    return! parser tail args
                }

            match result with
            | Ok (tail, args) -> Args.parseRec tail args
            | Error e -> Error e
        | Empty -> Ok args


    static member parse(cmdline: seq<string>): Result<Args, string> =
        Args.parseRec (cmdline |> Iterator) (Args.Default)
      
      static member test () = 4
