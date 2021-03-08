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
    |> Functional.collectWhile (fun elem ->
        match elem with
        | :? 'T as x -> Some x
        | _ -> None)

let termsFromIsqDocument (doc: IDocument): Result<List<Term>, string> =
    let termSelectorOption =
        doc.QuerySelector "body"
        |> querySelector "select[id='TERM_ID']"

    Functional.result {
        let! termSelector =
            termSelectorOption
            |> Functional.optionToResult "No select[id='TERM_ID'] found in the <body>"

        let! options =
            termSelector
            |> querySelectorAll<IHtmlOptionElement> ("option")
            |> Functional.optionToResult
                "Not all 'option' children of select[id='TERM_ID'] were IHtmlOptionElement. This should never happen."

        let! terms =
            options
            |> Functional.collectResultWhile (fun option ->
                (Term.fromIdAndString option.Value option.Text)
                |> Functional.optionToResult ("Term '" + option.Text + "' is invalid."))

        return terms
    }

let private postBody (term: Term) =
    "pv_sub=Submit&pv_term=" + term.id + "&pv_dept="

let getIsqHtmlWithTerm (post: Http.BodyFunction, term: Term) =
    post "https://bannerssb.unf.edu/nfpo-ssb/wkshisq.p_isq_dept_pub" (postBody term) "text/html;charset=UTF-8"

let departmentsFromIsqTermDocument (doc: IDocument): Result<List<Department>, string> =
    let deptSelectorOption =
        doc.QuerySelector "body"
        |> querySelector "select[id='DEPT_ID']"

    Functional.result {
        let! deptSelector =
            deptSelectorOption
            |> Functional.optionToResult "No select[id='DEPT_ID'] found in the <body>"

        let! options =
            deptSelector
            |> querySelectorAll<IHtmlOptionElement> ("option")
            |> Functional.optionToResult
                "Not all 'option' children of select[id='DEPT_ID'] were IHtmlOptionElement. This should never happen."

        return
            options
            |> List.map (fun option -> {id = option.Value; name = option.Text})
    }

let private isqUrl (term: Term) (dept: Department) =
    "https://bannerssb.unf.edu/nfpo-ssb/wkshisq.csv?pv_rpt=ISQ_Dept_Summary_Data&pv_term="
    + term.id
    + "&pv_dept="
    + dept.id
    + "6202&pv_pidm="

let getIsqCsv (get: Http.GetFunction) (term: Term) (department: Department) =
    get (isqUrl term department)
