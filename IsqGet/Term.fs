module IsqGet.Term

open System
open System.Numerics

type Season =
    | Spring = 0
    | Summer = 1
    | Fall = 2

type Term = { id: int; year: int; season: Season }

let parseSeason (str: string) =
    match str.ToLower() with
    | "spring" -> Some Season.Spring
    | "summer" -> Some Season.Summer
    | "fall" -> Some Season.Fall
    | _ -> None

let fromIdAndString (id: int) (str: string) =
    match str.Split(" ") with
    | [| season; year |] ->
        parseSeason season
        |> Option.bind (fun season ->
            Functional.stringToInt year
            |> Option.map (fun year ->
                { id = id
                  year = year
                  season = season }))
    | _ -> None
