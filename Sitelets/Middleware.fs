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
open Microsoft.AspNetCore.Mvc
open Microsoft.AspNetCore.Routing
open Microsoft.AspNetCore.Mvc.Abstractions
open System.Runtime.CompilerServices
open Microsoft.AspNetCore.Builder
open System.Text.Json

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
                let req = httpCtx.Request
                match sitelet.Router.Route req with
                | Some endpoint ->
                    let ctx = SiteletHelper.createContext sitelet httpCtx
                    let content = sitelet.Controller ctx endpoint
                    SiteletHelper.contentHelper httpCtx content
                    |> Async.StartAsTask :> Task
                | None -> next.Invoke()
            )

[<Extension>]
type ApplicationBuilderExtensions =
    [<Extension>]
    static member UseSitelets(this: IApplicationBuilder, ?sitelet: Sitelet<'T>) =
        this.Use(Middleware.Middleware (sitelet |> Option.map (fun s -> Sitelet.Box s)))