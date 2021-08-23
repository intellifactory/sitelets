module Sitelets.Test.App

open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Sitelets
open Microsoft.AspNetCore.Http
open System.Threading.Tasks
open Microsoft.AspNetCore.Mvc
open Microsoft.AspNetCore.Routing
open Microsoft.AspNetCore.Mvc.Abstractions
open System.Collections.Generic

// ---------------------------------
// Models
// ---------------------------------

type Message =
    {
        Text : string
    }

// ---------------------------------
// Views
// ---------------------------------

module Views =
    open Giraffe.ViewEngine

    let layout (content: XmlNode list) =
        html [] [
            head [] [
                title []  [ encodedText "Sitelets.Test" ]
                link [ _rel  "stylesheet"
                       _type "text/css"
                       _href "/main.css" ]
            ]
            body [] content
        ]

    let partial () =
        h1 [] [ encodedText "Sitelets.Test" ]

    let index (model : Message) =
        [
            partial()
            p [] [ encodedText model.Text ]
        ] |> layout

// ---------------------------------
// Web app
// ---------------------------------

let indexHandler (name : string) =
    let greetings = sprintf "Hello %s, from Giraffe!" name
    let model     = { Text = greetings }
    let view      = Views.index model
    htmlView view

// ---------------------------------
// Sitelet Test
// ---------------------------------

type Endpoint =
    | [<EndPoint "GET /echo">] Str of string
    | [<EndPoint "GET /actionresult">] ActionResult
    | [<EndPoint "GET /task/str">] Task1
    | [<EndPoint "GET /task/actionresult">] Task2
    | [<EndPoint "GET /task/enumerable">] Task3
    | [<EndPoint "GET /enum">] Enumerable1
    | [<EndPoint "GET /enum/async">] Enumerable2
    | Test1
    | Test2

let mysitelet = 
    Sitelet.Sum [
        Sitelet.Content "/sitelets" Test1 (fun ctx -> box "Hello World")
        Sitelet.Content "/sitelets2" Test2 (fun ctx -> box "Hello World2")
        Sitelet.Infer (fun ctx -> function
            | Str s -> box s
            | ActionResult -> box Ok
            | Task1 -> box <| Task.FromResult "task1"
            | Task2 ->
                let contentRes = ContentResult(Content = "Hello World")
                Task.FromResult contentRes
                |> box
            | Task3 ->
                let e : seq<string * string> = seq{("hihi", "haha")}
                Task.FromResult e
                |> box
            | Enumerable1 ->
                seq{("haha", "huhu")}
                |> box
            | Enumerable2 ->
                let asyncEnum : IAsyncEnumerable<int> = System.Linq.AsyncEnumerable.Empty<int>()
                asyncEnum
                |> box
            | _ -> failwith "error"
        )
    ]

let webApp =
    choose [
        GET >=>
            choose [
                route "/" >=> indexHandler "world"
                routef "/hello/%s" indexHandler
                SiteletHelper.sitelet mysitelet
            ]
        setStatusCode 404 >=> text "Not Found" ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureCors (builder : CorsPolicyBuilder) =
    builder
        .WithOrigins(
            "http://localhost:5000",
            "https://localhost:5001")
       .AllowAnyMethod()
       .AllowAnyHeader()
       |> ignore

let configureApp (app : IApplicationBuilder) =
    let env = app.ApplicationServices.GetService<IWebHostEnvironment>()
    (match env.IsDevelopment() with
    | true  ->
        app.UseDeveloperExceptionPage()
    | false ->
        app .UseGiraffeErrorHandler(errorHandler)
            .UseHttpsRedirection())
        .UseCors(configureCors)
        .UseStaticFiles()
        .UseGiraffe(webApp)

let configureServices (services : IServiceCollection) =
    services.AddCors()    |> ignore
    services.AddGiraffe() |> ignore
    services.AddMvc()     |> ignore

let configureLogging (builder : ILoggingBuilder) =
    builder.AddConsole()
           .AddDebug() |> ignore

[<EntryPoint>]
let main args =
    let contentRoot = Directory.GetCurrentDirectory()
    let webRoot     = Path.Combine(contentRoot, "WebRoot")
    Host.CreateDefaultBuilder(args)
        .ConfigureWebHostDefaults(
            fun webHostBuilder ->
                webHostBuilder
                    .UseContentRoot(contentRoot)
                    .UseWebRoot(webRoot)
                    .Configure(Action<IApplicationBuilder> configureApp)
                    .ConfigureServices(configureServices)
                    .ConfigureLogging(configureLogging)
                    |> ignore)
        .Build()
        .Run()
    0