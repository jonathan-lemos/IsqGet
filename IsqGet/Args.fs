namespace IsqGet

open System.Collections.Generic
open IsqGet
open IsqGet.Iterator

type Outputter = seq<string> -> Result<unit, string>

type private TermSegmentParseResult =
    | Year of int
    | TermValue of Term
    | Invalid

type Range(lower: Term, upper: Term) =
    let (lower, upper) = if lower > upper then (upper, lower) else (lower, upper)
    member this.includes(term: Term): bool = lower <= term && term <= upper
    
    static member private parseSegment (segment: string): TermSegmentParseResult =
        match segment.ToLower() with
        | Str.ParseRegex "^\\d+$" _matches -> Year (int segment)
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
            | Some season -> TermValue (Term(season, int year))
            | None -> Invalid
        | _ -> Invalid
               
    static member private parseRange (segmentBegin: string) (segmentEnd: string): Range option =
        let tuple =
            match (Range.parseSegment segmentBegin, Range.parseSegment segmentEnd) with
            | (Year first, Year last) ->
                let (first, last) = if first > last then (last, first) else (first, last)
                Some (Term(Spring, first), Term(Fall, last))
            | (TermValue first, TermValue last) ->
                Some (if first > last then (last, first) else (first, last))
            | _ -> None
        
        tuple |> Option.map Range
        
    static member private parseSingle (segment: string): Range option =
        let tuple =
            match Range.parseSegment segment with
            | Year year -> Some (Term(Spring, year), Term(Fall, year))
            | TermValue term -> Some (term, term)
            | Invalid -> None
        
        tuple |> Option.map Range
    
    static member fromString (string: string): Range option =
        match string with
        | Str.ParseRegex "^(.+)-(.+)$" [first; second] -> Range.parseRange first second
        | _ -> Range.parseSingle string
    

type Args(serializer: Serialize.Serializer,
          professors: string list,
          courses: string list,
          termRanges: Range list,
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

    member this.matchesTerm(term: Term) =
        if termRanges.IsEmpty then
            true
        else
            termRanges
            |> Seq.exists (fun range -> range.includes term)

    member this.withSerializer(newSerializer: Serialize.Serializer): Args =
        Args(newSerializer, professors, courses, termRanges, outputter)

    member this.withProfessor(professor: string): Args =
        Args(serializer, professor :: professors, courses, termRanges, outputter)

    member this.withCourse(course: string): Args =
        Args(serializer, professors, course :: courses, termRanges, outputter)

    member this.withTermRange(range: Range): Args =
        Args(serializer, professors, courses, range :: termRanges, outputter)

    member this.withOutputter(newOutputter: Outputter): Args =
        Args(serializer, professors, courses, termRanges, newOutputter)
           
    static member printHelp(progName: string) =
        printfn "Usage: %s [...Args?]" progName
        printfn "Args:"
        printfn "\t--professor [professor]"
        printfn "\t\tInclude only the given professor. Can be given more than once to return multiple professors."
        printfn "\t--course [course]"

        printfn
            "\t\tInclude only the given course name/course code. Can be given more than once to return multiple courses."

        printfn "\t--term [2012 or Spring2012 or 2012-2015 or Spring2012-Fall2015]"
        printfn "\t\tInclude only the given term(s). Can be given more than once to return multiple terms."

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
                Functional.result {
                    let! parser =
                        match head with
                        | "--professor" -> Ok Args.parseProfessor
                        | "--course" -> Ok Args.parseCourse
                        | "--term" -> Ok Args.parseTermRange
                        | _ -> Error(sprintf "Unrecognized option '%s'" head)

                    return! parser tail args
                }

            match result with
            | Ok (tail, args) -> Args.parseRec tail args
            | Error e -> Error e
        | Empty -> Ok args


    static member parse(cmdline: seq<string>): Result<Args, string> =
        Args.parseRec (cmdline |> Iterator) (Args())
