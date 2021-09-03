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

module UrlHelpers = // HttpTODO
    open System
    open System.Text.RegularExpressions
    open Microsoft.AspNetCore.Http

    module Internals =
        let matchToken pattern s : (string * string) option =
            let regexp = new Regex("^(" + pattern + ")(.*)")
            let results = regexp.Match s
            if results.Success then
                (results.Groups.[1].Value, results.Groups.[2].Value) |> Some
            else
                None

        let MatchToken s f pattern =
            s |> matchToken pattern |> Option.bind f

        let MatchSymbol s pattern =
            pattern |> MatchToken s (fun (_, rest) -> rest |> Some)

    let (|EOL|_|) s =
        @"$" |> Internals.MatchToken s
            (fun (n, rest) -> Some ())

    let (|SLASH|_|) s =
        @"/" |> Internals.MatchToken s
            (fun (n, rest) -> rest |> Some)

    let (|INT|_|) (s: string) =
        let res = ref 0
        if Int32.TryParse(s, res) then
            !res |> Some
        else
            None
    
    let (|UINT|_|) (s: string) =
        let res = ref 0u
        if UInt32.TryParse(s, res) then
            !res |> Some
        else
            None
     
    let (|INT64|_|) (s: string) =
        let res = ref 0L
        if Int64.TryParse(s, res) then
            !res |> Some
        else
            None

    let (|UINT64|_|) (s: string) =
        let res = ref 0UL
        if UInt64.TryParse(s, res) then
            !res |> Some
        else
            None
    
    let (|INT16|_|) (s: string) =
        let res = ref (int16 0)
        if Int16.TryParse(s, res) then
            !res |> Some
        else
            None

    let (|UINT16|_|) (s: string) =
        let res = ref (uint16 0)
        if UInt16.TryParse(s, res) then
            !res |> Some
        else
            None
    
    let (|INT8|_|) (s: string) =
        let res = ref (int8 0)
        if SByte.TryParse(s, res) then
            !res |> Some
        else
            None
    
    let (|UINT8|_|) (s: string) =
        let res = ref (uint8 0)
        if Byte.TryParse(s, res) then
            !res |> Some
        else
            None

    let (|SBYTE|_|) (s: string) =
        let res = ref (sbyte 0)
        if SByte.TryParse(s, res) then
            !res |> Some
        else
            None
    
    let (|BYTE|_|) (s: string) =
        let res = ref (byte 0)
        if Byte.TryParse(s, res) then
            !res |> Some
        else
            None
            
    let (|FLOAT|_|) (s: string)=
        let res = ref 0.0
        if Double.TryParse(s, res) then
            !res |> Some
        else
            None

    let (|GUID|_|) (s: string) =
        let res = ref Guid.Empty
        if Guid.TryParse(s, res) then
            !res |> Some
        else
            None

    let (|BOOL|_|) (s: string) =
        let res = ref false
        if Boolean.TryParse(s, res) then
            !res |> Some
        else
            None

    let (|ALPHA|_|) s =
        "[a-zA-Z]+" |> Internals.MatchToken s (fun res -> res |> Some)

    let (|ALPHA_NUM|_|) s =
        "[a-zA-Z0-9]+" |> Internals.MatchToken s (fun res -> res |> Some)

    let (|REGEX|_|) regexp input =
        regexp |> Internals.MatchToken input
            (fun (n, rest) -> Some rest)

    let (|SPLIT_BY|_|) (c: char) (uri: string) =
        uri.Split c
        |> Array.filter (fun s -> s <> "")
        |> List.ofArray
        |> Some

    let (|DELETE|GET|OPTIONS|POST|PUT|TRACE|SPECIAL|) (req: HttpRequest) =
        let allParams () =
            let queryList =
                HttpHelpers.CollectionToMap req.Query
                |> Map.toList
            let formList =
                HttpHelpers.CollectionToMap req.Form
                |> Map.toList
            queryList @ formList
                
        match req.Method with
        | "DELETE" ->
            DELETE (allParams (), req.PathBase.ToUriComponent())
        | "GET" ->
            GET (HttpHelpers.CollectionToMap req.Query |> Map.toList, req.PathBase.ToUriComponent())
        | "OPTIONS" ->
            OPTIONS (allParams (), req.PathBase.ToUriComponent())
        | "POST" ->
            POST (HttpHelpers.CollectionToMap req.Form |> Map.toList, req.PathBase.ToUriComponent())
        | "PUT" ->
            PUT (allParams (), req.PathBase.ToUriComponent())
        | "TRACE" ->
            TRACE (allParams (), req.PathBase.ToUriComponent())
        // TODO: Revise.  Unfortunately, F# active patterns only allow up to 7 cases.
        | _ ->
            SPECIAL (req.Method, allParams, req.PathBase.ToUriComponent())

