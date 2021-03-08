module IsqGet.Http

open System.Net.Http
open System.Text

type GetFunction = string -> Async<Result<string, HttpRequestException>>

let get (url: string): Async<Result<string, HttpRequestException>> =
    use client = new HttpClient()
    client.DefaultRequestHeaders.Add("User-Agent", "Mozilla/5.0 (Windows NT 10.0; rv:78.0) Gecko/20100101 Firefox/78.0")

    async {
        try
            let! result = (client.GetAsync url) |> Async.AwaitTask

            let! content =
                result.Content.ReadAsStringAsync()
                |> Async.AwaitTask

            return Ok(content)
        with :? HttpRequestException as e -> return Error(e)
    }

type BodyFunction = string -> string -> string -> Async<Result<string, HttpRequestException>>

let post (url: string) (body: string) (contentType: string): Async<Result<string, HttpRequestException>> =
    use client = new HttpClient()
    client.DefaultRequestHeaders.Add("User-Agent", "Mozilla/5.0 (Windows NT 10.0; rv:78.0) Gecko/20100101 Firefox/78.0")

    async {
        try
            let! result =
                (client.PostAsync(url, new StringContent(body, Encoding.UTF8, contentType)))
                |> Async.AwaitTask

            let! content =
                result.Content.ReadAsStringAsync()
                |> Async.AwaitTask

            return Ok(content)
        with :? HttpRequestException as e -> return Error(e)
    }
