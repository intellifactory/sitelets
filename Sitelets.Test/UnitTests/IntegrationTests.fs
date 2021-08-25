module IntegrationTests

open System
open System.Collections.Generic
open System.Linq
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.HttpsPolicy;
open Microsoft.AspNetCore.Mvc
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Sitelets
open NUnit.Framework
open FsUnit
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Mvc.Testing
open Sitelets
open System.Text.Json
open System.Text.Json.Serialization

let options = JsonSerializerOptions()
options.Converters.Add(JsonFSharpConverter())

let webAppFactory = new WebApplicationFactory<ANCFSharp.Startup>()

let client = webAppFactory.CreateClient()

[<Test; Category("Hello World Tests")>]
let ``Hello World integration test`` () =
    async {
        let! resp = client.GetAsync("/hello") |> Async.AwaitTask
        resp.IsSuccessStatusCode |> should be True

        let! resp = client.GetAsync("/nothello") |> Async.AwaitTask
        resp.IsSuccessStatusCode |> should be False
    }
    |> Async.StartAsTask

[<Test; Category("Sitelet Tests")>]
let ``Content test`` () =

    async {
        let! resp = client.GetAsync("/hello") |> Async.AwaitTask
        let! str = resp.Content.ReadAsStringAsync() |> Async.AwaitTask
        str |> should equal "Hello World"
    }
    |> Async.StartAsTask

[<Test; Category("Sitelet Tests")>]
let ``MapContent test`` () =

    async {
        let! resp = client.GetAsync("/mapped") |> Async.AwaitTask
        let! str = resp.Content.ReadAsStringAsync() |> Async.AwaitTask
        str |> should equal "Hello again"
    }
    |> Async.StartAsTask

[<Test; Category("Sitelet Tests")>]
let ``Json api test`` () =

    async {
        let post : TestSitelets.RecTest = {A = "hello"; B = 123; C = true}
        let content = System.Net.Http.Json.JsonContent.Create<TestSitelets.RecTest>(post, options = options)
        let! resp = client.PostAsync("/json", content) |> Async.AwaitTask
        resp.IsSuccessStatusCode |> should be True
        let! str = resp.Content.ReadAsStringAsync() |> Async.AwaitTask
        let record =
            System.Text.Json.JsonSerializer.Deserialize<TestSitelets.RecTest>(str, options)
        record.A |> should equal "hello"
        record.B |> should equal 123
        record.C |> should equal true
    }
    |> Async.StartAsTask

[<Test; Category("Sitelet Tests")>]
let ``Protect test`` () =
    async {
        client.DefaultRequestHeaders.Add("X-base-Token", ["23443"; "Tamas"; "asd@gamil.com"])
        let! resp = client.GetAsync("/protected") |> Async.AwaitTask
        resp.IsSuccessStatusCode |> should be True
        let! content = resp.Content.ReadAsStringAsync() |> Async.AwaitTask
        content |> should equal "23443"
    }
    |> Async.StartAsTask