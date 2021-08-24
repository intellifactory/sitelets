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

[<Test>]
let ``Hello World integration test`` () =
    let webAppFactory = new WebApplicationFactory<ANCFSharp.Startup>()
    
    let client = webAppFactory.CreateClient()

    async {
        let! resp = client.GetAsync("/hello") |> Async.AwaitTask
        resp.IsSuccessStatusCode |> should be True

        let! resp = client.GetAsync("/nothello") |> Async.AwaitTask
        resp.IsSuccessStatusCode |> should be False
    }
    |> Async.StartAsTask
