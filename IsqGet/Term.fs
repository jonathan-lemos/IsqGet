namespace IsqGet

open System

type Season =
    | Spring
    | Summer
    | Fall   

type Term(year: int, season: Season, id: string) =
    member this.id = id
    member this.year = year
    member this.season = season
    
    new (season: Season, year: int) = Term(year, season, "")
    
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

                return Term(year, season, id)
            }
        | _ -> None
        
    interface IComparable<Term> with
        member this.CompareTo(other) =
            compare (this.year, this.season) (other.year, other.season)
            
    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Term as term -> (this :> IComparable<Term>).CompareTo term
            | _ -> raise (ArgumentException "Argument was not Term")
            
    override this.Equals(obj) =
        match obj with
        | :? Term as term -> (this :> IComparable<Term>).CompareTo term = 0
        | _ -> false
        
    override this.GetHashCode() =
        hash (this.year, this.season)
            