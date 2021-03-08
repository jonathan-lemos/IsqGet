namespace IsqGet

type Season =
    | Spring = 0
    | Summer = 1
    | Fall = 2

type Term(id: string, year: int, season: Season) =
    member this.id = id
    member this.year = year
    member this.season = season
    
    static member parseSeason(str: string): Season option =
        match str.ToLower() with
        | "spring" -> Some Season.Spring
        | "summer" -> Some Season.Summer
        | "fall" -> Some Season.Fall
        | _ -> None

    static member fromIdAndString (id: string) (str: string): Term option =
        match str.Split(" ") with
        | [| seasonStr; yearStr |] ->
            Functional.option {
                let! season = Term.parseSeason seasonStr
                let! year = Functional.stringToInt yearStr

                return Term(id, year, season)
            }
        | _ -> None
