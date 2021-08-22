// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

module SPA =
    type EndPoint =
        | Home

open System
open System.Runtime.CompilerServices
open System.Threading.Tasks
open Microsoft.AspNetCore.Http

[<Class>]
type Application =

    static member BaseMultiPage f = Sitelet.Infer f

    static member BaseSinglePage (f: Context<SPA.EndPoint> -> obj) =
        {
            Router = Router.Single SPA.EndPoint.Home "/"
            Controller = fun ctx _ -> f ctx
        }

    static member SinglePage (f: Func<Context<SPA.EndPoint>, obj>) : Sitelet<SPA.EndPoint> =
        Application.BaseSinglePage (fun ctx -> f.Invoke ctx)

    static member MultiPage (f: Func<Context<'EndPoint>, 'EndPoint, obj>) : Sitelet<'EndPoint> =
        Application.BaseMultiPage (fun ctx ep -> f.Invoke(ctx, ep))

    static member Text (f: Func<Context<SPA.EndPoint>, string>) : Sitelet<SPA.EndPoint> =
        Application.BaseSinglePage (fun ctx ->
            f.Invoke ctx |> box
        )
