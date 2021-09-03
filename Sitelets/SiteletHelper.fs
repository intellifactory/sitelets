﻿// $begin{copyright}
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



module SiteletHelper =
    open Sitelets

    open System.Threading.Tasks
    open Microsoft.AspNetCore.Mvc
    open Microsoft.AspNetCore.Mvc.Abstractions
    open Microsoft.AspNetCore.Routing
    open Microsoft.AspNetCore.Http
    open Microsoft.Extensions.Options
    open System.Text.Json
    
    type SiteletHttpFuncResult = Task<HttpContext option>
    type SiteletHttpFunc =  HttpContext -> SiteletHttpFuncResult
    type SiteletHttpHandler = SiteletHttpFunc -> SiteletHttpFunc


    let rec internal contentHelper (httpCtx: HttpContext) (content: obj) =
        async {
            match content with
            | :? string as stringContent ->
                httpCtx.Response.StatusCode <- StatusCodes.Status200OK
                do! httpCtx.Response.WriteAsync(stringContent) |> Async.AwaitTask
                return None
            | :? IActionResult as actionResult ->
                let actionCtx = ActionContext(httpCtx, RouteData(), ActionDescriptor())
                do! actionResult.ExecuteResultAsync(actionCtx) |> Async.AwaitTask
                return None
            | _ ->
                let contentType = content.GetType()
                if contentType.IsGenericType && contentType.GetGenericTypeDefinition() = typedefof<Task<_>> then
                    let contentTask = content :?> Task
                    do! contentTask |> Async.AwaitTask
                    let contentResult =
                        let resultGetter = contentType.GetProperty("Result")
                        resultGetter.GetMethod.Invoke(contentTask, [||])
                    return! contentHelper httpCtx contentResult
                else
                    httpCtx.Response.StatusCode <- StatusCodes.Status200OK
                    let jsonOptions = httpCtx.RequestServices.GetService(typeof<IOptions<JsonSerializerOptions>>) :?> IOptions<JsonSerializerOptions>;
                    do! System.Text.Json.JsonSerializer.SerializeAsync(httpCtx.Response.Body, content, jsonOptions.Value) |> Async.AwaitTask
                    return None
        }
    
    let private (++) (a: string) (b: string) =
        let startsWithSlash (s: string) =
            s.Length > 0
            && s.[0] = '/'
        let endsWithSlash (s: string) =
            s.Length > 0
            && s.[s.Length - 1] = '/'
        match endsWithSlash a, startsWithSlash b with
        | true, true -> a + b.Substring(1)
        | false, false -> a + "/" + b
        | _ -> a + b

    let internal createContext (sl: Sitelet<'T>) (httpCtx: HttpContext) =
        
        let appPath = httpCtx.Request.PathBase.ToUriComponent()
        let link (x: 'T) =
            match sl.Router.Link x with
            | None -> failwithf "Failed to link to %O" (box x)
            | Some loc when HttpHelpers.IsAbsoluteUrl loc -> loc
            | Some loc -> appPath ++ loc

        {
            Link = link
            HttpContext = httpCtx
        }

    let sitelet (sl : Sitelet<'T>) : SiteletHttpHandler =
        fun (next: SiteletHttpFunc) ->
            let handleSitelet (httpCtx: HttpContext) =
                let req = RoutedHttpRequest httpCtx.Request :> IHttpRequest

                let handleRouterResult r =
                    match r with
                    | Some endpoint ->
                        let ctx = createContext sl httpCtx
                        let content = sl.Controller ctx endpoint
                        contentHelper httpCtx content
                        |> Async.StartAsTask
                    | None ->
                        next httpCtx
                
                let routeWithoutBody =
                    try
                        Some (sl.Router.Route req)
                    with :? Router.BodyTextNeededForRoute ->
                        None 

                match routeWithoutBody with
                | Some r ->
                    handleRouterResult r
                | None -> 
                    async {
                        do! req.BodyText |> Async.AwaitTask |> Async.Ignore
                        let routeWithBody = sl.Router.Route req
                        return! handleRouterResult routeWithBody |> Async.AwaitTask  
                    }
                    |> Async.StartAsTask

            handleSitelet