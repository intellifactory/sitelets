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

/// Defines HTTP-related functionality.
module HttpHelpers =
    open System.Collections.Generic
    open Microsoft.AspNetCore.Http

    type SV = Microsoft.Extensions.Primitives.StringValues

    let CollectionToMap<'T when 'T :> IEnumerable<KeyValuePair<string, SV>>> (c: 'T) : Map<string, string> =
        c
        |> Seq.map (fun i ->
            i.Key, i.Value.ToString()
        )
        |> Map.ofSeq

    let CollectionToMap2<'T when 'T :> IEnumerable<KeyValuePair<string, string>>> (c: 'T) : Map<string, string> =
        c
        |> Seq.map (fun i ->
            i.Key, i.Value.ToString()
        )
        |> Map.ofSeq
    
    let CollectionFromMap<'T when 'T :> IEnumerable<KeyValuePair<string, SV>>> (m: Map<string, string>) : IEnumerable<KeyValuePair<string, SV>> =
        m
        |> Seq.map (fun (KeyValue(k,v)) ->
            KeyValuePair (k, Microsoft.Extensions.Primitives.StringValues v)
            )

[<AutoOpen>]
module RoutedRequest =
    open System.Collections.Generic
    open Microsoft.AspNetCore.Http
    open HttpHelpers
    open System.Threading.Tasks

    type IHttpRequest =
        abstract member Body: string
        abstract member Cookies: Map<string, string>
        abstract member Form: Map<string, string>
        abstract member Headers: Map<string, string>
        abstract member Host: string
        abstract member Method: string
        abstract member Path: string
        abstract member PathBase: string
        abstract member Query: Map<string, string>
        abstract member Scheme: string

    type RoutedHttpRequest (req: HttpRequest) =
        let mutable bodyText = null : Task<string>

        interface IHttpRequest with
            override x.Body =
                x.BodyText
            override x.Cookies = CollectionToMap2 req.Cookies
            override x.Form = CollectionToMap req.Form
            override x.Headers = CollectionToMap req.Headers
            override x.Host = req.Host.ToString()
            override x.Method = req.Method
            override x.Path = req.Path.ToString()
            override x.PathBase = req.PathBase.ToString()
            override x.Query = CollectionToMap req.Query
            override x.Scheme = req.Scheme

        member x.BodyText =
            x.BodyTextAsync |> Async.RunSynchronously

        member x.BodyTextAsync =
            if isNull bodyText then
                let i = req.Body
                if isNull i then
                    bodyText <- Task.FromResult ""    
                else
                    let reader = new System.IO.StreamReader(i, System.Text.Encoding.UTF8, false, 1024, leaveOpen = true)
                    bodyText <- reader.ReadToEndAsync()
            bodyText |> Async.AwaitTask

    type RHRWithPath (ireq: IHttpRequest, p: string) =
        interface IHttpRequest with
            override x.Body = ireq.Body
            override x.Cookies = ireq.Cookies
            override x.Form = ireq.Form
            override x.Headers = ireq.Headers
            override x.Host = ireq.Host
            override x.Method = ireq.Method
            override x.Path = p
            override x.PathBase = ireq.PathBase
            override x.Query = ireq.Query
            override x.Scheme = ireq.Scheme