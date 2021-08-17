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

module SiteletHelper =
    open Sitelets

    open System.Text.Json

    open System.Threading.Tasks
    open Microsoft.AspNetCore.Mvc
    open Microsoft.AspNetCore.Mvc.Abstractions
    open Microsoft.AspNetCore.Routing
    open Microsoft.AspNetCore.Http
    
    type SiteletHttpFuncResult = Task<HttpContext option>
    type SiteletHttpFunc =  HttpContext -> SiteletHttpFuncResult
    type SiteletHttpHandler = SiteletHttpFunc -> SiteletHttpFunc

    let sitelet (sl : Sitelet<'T>) : SiteletHttpHandler =
        fun (httpFunc: SiteletHttpFunc) ->
            let handleSitelet (httpCtx: HttpContext) =
                let rec contentHelper (endpoint: 'T) =
                    async {
                        let content = sl.Controller httpCtx endpoint
                        match content with
                        | :? string as stringContent ->
                            httpCtx.Response.StatusCode <- StatusCodes.Status200OK
                            do! httpCtx.Response.WriteAsync(stringContent) |> Async.AwaitTask
                            return None
                        | :? IActionResult as actionResult ->
                            let actionCtx = ActionContext(httpCtx, RouteData(), ActionDescriptor())
                            do! actionResult.ExecuteResultAsync(actionCtx) |> Async.AwaitTask
                            return None
                        | :? Task<_> as t ->
                            let! result = t |> Async.AwaitTask
                            return! contentHelper result
                        | _ ->
                            httpCtx.Response.StatusCode <- StatusCodes.Status200OK
                            //do! httpCtx.Response.WriteAsJsonAsync(content) |> Async.AwaitTask
                            use s = new System.IO.MemoryStream()
                            do! httpCtx.Response.Body.CopyToAsync(s) |> Async.AwaitTask
                            let json = s.ToString()
                            do! httpCtx.Response.WriteAsync(json) |> Async.AwaitTask
                            return None
                    }
                let req = httpCtx.Request
                match sl.Router.Route req with
                | Some endpoint ->
                    contentHelper endpoint
                    |> Async.StartAsTask
                | None -> Task.FromResult (Some httpCtx)
                
            handleSitelet