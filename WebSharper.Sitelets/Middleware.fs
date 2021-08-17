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
open System.IO
open System.Threading.Tasks
open Microsoft.AspNetCore.Http

module Middleware =
    let private writeResponseAsync (resp: HttpResponse) (out: HttpResponse) : Async<unit> =
        async {
            use memStr = new MemoryStream()
            do
                out.StatusCode <- resp.StatusCode
                for h in resp.Headers do
                    out.Headers.Add(h.Key, h.Value)
                resp.Body.CopyToAsync(memStr) |> Async.AwaitTask |> ignore
                memStr.Seek(0L, SeekOrigin.Begin) |> ignore
            do! memStr.CopyToAsync(out.Body) |> Async.AwaitTask    
        }

    let Middleware (options: WebSharperOptions) =
        let sitelet =
            match options.Sitelet with
            | Some s -> Some s
            | None -> Loading.DiscoverSitelet options.Assemblies
        match sitelet with
        | None ->
            Func<_,_,_>(fun (_: HttpContext) (next: Func<Task>) -> next.Invoke())
        | Some sitelet ->
            Func<_,_,_>(fun (httpCtx: HttpContext) (next: Func<Task>) ->
                let ctx = Context.GetOrMake httpCtx options
                match sitelet.Router.Route ctx.Request with
                | Some endpoint ->
                    async {
                        let content = sitelet.Controller httpCtx endpoint
                        let! response = Content.ToResponse content ctx
                        do! writeResponseAsync response httpCtx.Response
                    }
                    |> Async.StartAsTask :> Task
                | None -> next.Invoke()
            )
