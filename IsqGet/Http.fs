module IsqGet.Http

open System.Net.Http
open System.Text

type GetFunction = string -> Async<Result<string, string>>

let get (url: string): Async<Result<string, string>> =
    async {
        use client = new HttpClient()
        client.DefaultRequestHeaders.Add("User-Agent", "Mozilla/5.0 (Windows NT 10.0; rv:78.0) Gecko/20100101 Firefox/78.0")
        
        try
            let! result = (client.GetAsync url) |> Async.AwaitTask

            let! content =
                result.Content.ReadAsStringAsync()
                |> Async.AwaitTask

            return Ok content
        with :? HttpRequestException as e -> return Error e.Message
    }

type BodyFunction = string -> string -> string -> Async<Result<string, string>>

let post (url: string) (body: string) (contentType: string): Async<Result<string, string>> =
    async {    
        use client = new HttpClient()
        client.DefaultRequestHeaders.Add("User-Agent", "curl/7.75.0")
        
        try
            let requestContent = new StringContent(body, Encoding.UTF8)
            requestContent.Headers.Remove("Content-Type") |> ignore
            requestContent.Headers.TryAddWithoutValidation("Content-Type", contentType) |> ignore
            
            let! result =
                (client.PostAsync(url, requestContent))
                |> Async.AwaitTask

            let! content =
                result.Content.ReadAsStringAsync()
                |> Async.AwaitTask

            return Ok content
        with :? HttpRequestException as e -> return Error e.Message
    }
