// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}
namespace Sitelets

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open System.Runtime.CompilerServices
open Microsoft.AspNetCore.Routing


module Middleware =

    let Middleware (siteletOpt: option<Sitelet<obj>>) =
        let sitelet =
            match siteletOpt with
            | Some s -> Some s
            | None -> Loading.DiscoverSitelet ()
        match sitelet with
        | None ->
            Func<_,_,_>(fun (_: HttpContext) (next: Func<Task>) -> next.Invoke())
        | Some sitelet ->
            Func<_,_,_>(fun (httpCtx: HttpContext) (next: Func<Task>) ->
                
                let req = RoutedHttpRequest httpCtx.Request :> IHttpRequest

                let handleRouterResult r =
                    match r with
                    | Some endpoint ->
                        let ctx = SiteletHelper.createContext sitelet httpCtx
                        let content = sitelet.Controller ctx endpoint
                        SiteletHelper.contentHelper httpCtx content
                        |> Async.StartAsTask :> Task
                    | None -> next.Invoke()
                
                let routeWithoutBody =
                    try
                        Some (sitelet.Router.Route req)
                    with :? Router.BodyTextNeededForRoute ->
                        None 

                match routeWithoutBody with
                | Some r ->
                    handleRouterResult r
                | None -> 
                    async {
                        do! req.BodyText |> Async.AwaitTask |> Async.Ignore
                        let routeWithBody = sitelet.Router.Route req
                        do! handleRouterResult routeWithBody |> Async.AwaitTask  
                    }
                    |> Async.StartAsTask :> Task
            )

[<Extension>]
type ApplicationBuilderExtensions =
    [<Extension>]
    static member UseSitelets(this: IApplicationBuilder, ?sitelet: Sitelet<'T>) =
        this.Use(Middleware.Middleware (sitelet |> Option.map Sitelet.Box))

    [<Extension>]
    static member UseOpenApi(this: IEndpointRouteBuilder, endpointsType: Type, config: OpenApiIntegration.GenerateOpenApiConfig) =

        let responseBytes = OpenApiIntegration.generateSwaggerJsonBytes config endpointsType

        let getSwaggerJson (ctx : HttpContext) =
            async {
                ctx.Response.Headers.[Microsoft.Net.Http.Headers.HeaderNames.ContentType] <- Microsoft.Extensions.Primitives.StringValues("application/json")
                do! ctx.Response.Body.WriteAsync(responseBytes, 0, responseBytes.Length) |> Async.AwaitTask
            }
            |> Async.StartAsTask :> Task

        this.MapGet("swagger.json", new RequestDelegate(getSwaggerJson)) |> ignore
