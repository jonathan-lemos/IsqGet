module IsqGet.Fetch

open AngleSharp
open AngleSharp.Dom
open AngleSharp.Html.Dom
open IsqGet.Functional.Builders
open IsqGet.Functional.Convert
open IsqGet.Functional.Transform

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
    |> mapWhileIntoOption (fun elem ->
        match elem with
        | :? 'T as x -> Some x
        | _ -> None)

let termsFromIsqDocument (doc: IDocument): Result<List<Term>, string> =
    let termSelectorOption =
        doc.DocumentElement
        |> querySelector "select[id='TERM_ID']"

    result {
        let! termSelector =
            termSelectorOption
            |> optionToResult "No select[id='TERM_ID'] found in the <body>"

        let! options =
            termSelector
            |> querySelectorAll<IHtmlOptionElement> ("option")
            |> Option.map List.tail // skip the first 'None Selected' option
            |> optionToResult
                "Not all 'option' children of select[id='TERM_ID'] were IHtmlOptionElement. This should never happen."

        let! terms =
            options
            |> mapWhile (fun option ->
                (Term.fromIdAndString option.Value option.Text)
                |> optionToResult (sprintf "Term '%s' is invalid." option.Text))

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

    result {
        let! deptSelector =
            deptSelectorOption
            |> optionToResult "No select[id='DEPT_ID'] found in the <body>"

        let! options =
            deptSelector
            |> querySelectorAll<IHtmlOptionElement> ("option")
            |> Option.map List.tail // skip first 'All Departments' option
            |> optionToResult
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
