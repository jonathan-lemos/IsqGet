module IsqGet.Fetch

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
    |> Functional.mapWhileIntoOption (fun elem ->
        match elem with
        | :? 'T as x -> Some x
        | _ -> None)

let termsFromIsqDocument (doc: IDocument): Result<List<Term>, string> =
    let termSelectorOption =
        doc.DocumentElement
        |> querySelector "select[id='TERM_ID']"

    Functional.result {
        let! termSelector =
            termSelectorOption
            |> Functional.optionToResult "No select[id='TERM_ID'] found in the <body>"

        let! options =
            termSelector
            |> querySelectorAll<IHtmlOptionElement> ("option")
            |> Option.map List.tail // skip the first 'None Selected' option
            |> Functional.optionToResult
                "Not all 'option' children of select[id='TERM_ID'] were IHtmlOptionElement. This should never happen."

        let! terms =
            options
            |> Functional.mapWhile (fun option ->
                (Term.fromIdAndString option.Value option.Text)
                |> Functional.optionToResult ("Term '" + option.Text + "' is invalid."))

        return terms
    }

let private postBody (term: Term) =
    "pv_sub=Submit&pv_term=" + term.id + "&pv_dept="

let getIsqHtmlWithTerm (post: Http.BodyFunction) (term: Term) =
    post "https://bannerssb.unf.edu/nfpo-ssb/wkshisq.p_isq_dept_pub" (postBody term) "application/x-www-form-urlencoded"

let departmentsFromIsqTermDocument (doc: IDocument): Result<List<Department>, string> =
    let deptSelectorOption =
        doc.DocumentElement
        |> querySelector "select[id='DEPT_ID']"

    Functional.result {
        let! deptSelector =
            deptSelectorOption
            |> Functional.optionToResult "No select[id='DEPT_ID'] found in the <body>"

        let! options =
            deptSelector
            |> querySelectorAll<IHtmlOptionElement> ("option")
            |> Option.map List.tail // skip first 'All Departments' option
            |> Functional.optionToResult
                "Not all 'option' children of select[id='DEPT_ID'] were IHtmlOptionElement. This should never happen."

        return
            options
            |> List.map (fun option ->
                { id = option.Value
                  name = option.Text })
    }

let private isqUrl (term: Term) (dept: Department) =
    "https://bannerssb.unf.edu/nfpo-ssb/wkshisq.csv?pv_rpt=ISQ_Dept_Summary_Data&pv_term="
    + term.id
    + "&pv_dept="
    + dept.id
    + "&pv_pidm="

let getIsqCsv (get: Http.GetFunction) (term: Term) (department: Department) =
    async {
        let! result = get (isqUrl term department)

        return
            match result with
            | Ok "" -> Error(sprintf "Invalid term/dept combo %s/%s" term.id department.id)
            | Ok v -> Ok v
            | Error e -> Error e
    }
