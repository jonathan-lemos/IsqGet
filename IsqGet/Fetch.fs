module IsqGet.Fetch

open System.Linq
open AngleSharp
open AngleSharp.Dom
open AngleSharp.Html.Dom

let getIsqHtml (get: Http.GetFunction) =
    get "https://bannerssb.unf.edu/nfpo-ssb/wkshisq.p_isq_dept_pub"

let htmlToDocument html =
    let config = Configuration.Default
    let ctx = BrowsingContext.New(config)

    ctx.OpenAsync(fun req -> req.Content(html) |> ignore)
    |> Async.AwaitTask

let querySelector<'T when 'T :> IElement> (selector: string) (elem: IElement): Option<'T> =
    match elem.QuerySelector selector with
    | :? 'T as x -> Some x
    | _ -> None

let querySelectorAll<'T when 'T :> IElement> (selector: string) (elem: IElement): Option<List<'T>> =
    elem.QuerySelectorAll selector
    |> Functional.collectWhile (fun elem ->
        match elem with
        | :? 'T as x -> Ok x
        | _ -> Error())
    |> Functional.resultToOption


let termsFromIsqDocument (doc: IDocument) =
    let termSelectorElement = doc.QuerySelector "select[id='TERM_ID']"

    termSelectorElement
    |> querySelectorAll<IHtmlOptionElement> ("option")
    |> Option.bind (fun options ->
        options
        |> List.map (fun option ->
            let id = option.Value
            let term = option.Text
            term))
