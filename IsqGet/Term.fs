namespace IsqGet

type Season =
    | Spring
    | Summer
    | Fall

type Term(id: string, year: int, season: Season) =
    member this.id = id
    member this.year = year
    member this.season = season
    
    override this.ToString() =
        sprintf "%A %d" season year
          
    static member parseSeason(str: string): Season option =
        match str.ToLower() with
        | "spring" -> Some Season.Spring
        | "summer" -> Some Season.Summer
        | "fall" -> Some Season.Fall
        | _ -> None
        
    static member seasonToString (season: Season): string =
        match season with
        | Season.Spring -> "Spring"
        | Season.Summer -> "Summer"
        | Season.Fall -> "Fall"

    static member fromIdAndString (id: string) (str: string): Term option =
        match str.Split(" ") with
        | [| seasonStr; yearStr |] ->
            Functional.option {
                let! season = Term.parseSeason seasonStr
                let! year = Functional.stringToInt yearStr

                return Term(id, year, season)
            }
        | _ -> None
