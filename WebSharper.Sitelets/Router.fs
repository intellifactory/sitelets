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

open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text
open Microsoft.AspNetCore.Http

#nowarn "64" // type parameter renaming warnings 

/// Indicates the "Access-Control-Xyz" headers to send.
type CorsAllows =
    {
        Origins : string list
        Methods : string list
        Headers : string list
        ExposeHeaders : string list
        MaxAge : int option
        Credentials : bool
    }

    static member Empty =
        {
            Origins = []
            Methods = []
            Headers = []
            ExposeHeaders = []
            MaxAge = None
            Credentials = false
        }

/// Use as an endpoint to indicate that it must check for Cross-Origin Resource Sharing headers.
/// In addition to matching the same endpoints as 'EndPoint does, this also matches preflight OPTIONS requests.
/// The corresponding Content should use Content.Cors.
type Cors<'EndPoint> =
    {
        DefaultAllows : CorsAllows option
        // if None, then this is a preflight OPTIONS request
        EndPoint : 'EndPoint option
    }

module Cors =
    let Of (endpoint: 'EndPoint) =
        { DefaultAllows = None; EndPoint = Some endpoint }

    let (|Of|Preflight|) (c: Cors<'EndPoint>) =
        match c.EndPoint with
        | Some ep -> Of ep
        | None -> Preflight

[<RequireQualifiedAccess>]
type ParseRequestResult<'T> =
    | [<CompiledName "success">]
      Success of endpoint: 'T
    | [<CompiledName "invalidMethod">]
      InvalidMethod of endpoint: 'T * ``method``: string
    | [<CompiledName "invalidJson">]
      InvalidJson of endpoint: 'T
    | [<CompiledName "missingQueryParameter">]
      MissingQueryParameter of endpoint: 'T * queryParam: string
    | [<CompiledName "missingFormData">]
      MissingFormData of endpoint: 'T * formFieldName: string

    member this.Value =
        match this with
        | Success a
        | InvalidMethod (a, _)
        | InvalidJson a
        | MissingQueryParameter (a, _)
        | MissingFormData (a, _) -> a

    [<System.Obsolete "Use Value instead">]
    member this.Action = this.Value

[<System.Obsolete "Use ParseRequestResult instead of ActionEncoding.DecodeResult">]
/// For back-compatibility only, use ParseRequestResult instead of ActionEncoding.DecodeResult
module ActionEncoding =

    type DecodeResult<'T> = ParseRequestResult<'T>

    let Success endpoint = ParseRequestResult.Success endpoint
    let InvalidMethod (endpoint, ``method``) = ParseRequestResult.InvalidMethod(endpoint, ``method``)
    let InvalidJson endpoint = ParseRequestResult.InvalidJson endpoint
    let MissingQueryParameter (endpoint, queryParam) = ParseRequestResult.MissingQueryParameter(endpoint, queryParam)
    let MissingFormData (endpoint, formFieldName) = ParseRequestResult.MissingFormData(endpoint, formFieldName)

module StringEncoding =

    let isUnreserved isLast c =
        match c with
        | '-' | '_' -> true
        | '.' -> not isLast
        | c when c >= 'A' && c <= 'Z' -> true
        | c when c >= 'a' && c <= 'z' -> true
        | c when c >= '0' && c <= '9' -> true
        | _ -> false
    
    let writeEscaped (w: System.Text.StringBuilder) isLast c =
        let k = int c
        if isUnreserved isLast c then w.Append c
        elif k < 256 then w.AppendFormat("~{0:x2}", k)
        else w.AppendFormat("~u{0:x4}", k)
        |> ignore

    let write (s: string) = 
        let b = System.Text.StringBuilder()
        s |> Seq.iteri (fun i c ->
            writeEscaped b (i + 1 = s.Length) c)
        string b

    let inline ( ++ ) (a: int) (b: int) = (a <<< 4) + b

    [<Literal>]
    let EOF = -1

    [<Literal>]
    let ERROR = -2

    let readEscaped (r: System.IO.TextReader) =
        let hex x =
            match x with
            | x when x >= int '0' && x <= int '9' -> x - int '0'
            | x when x >= int 'a' && x <= int 'f' -> x - int 'a' + 10
            | x when x >= int 'A' && x <= int 'F' -> x - int 'A' + 10
            | _ -> ERROR
        match r.Read() with
        | x when x = int '~' ->
            match r.Read() with
            | x when x = int 'u' ->
                let a = r.Read()
                let b = r.Read()
                let c = r.Read()
                let d = r.Read()
                if a >= 0 && b >= 0 && c >= 0 && d >= 0 then
                    hex a ++ hex b ++ hex c ++ hex d
                else ERROR
            | x ->
                let y = r.Read()
                if x >= 0 && y >= 0 then
                    hex x ++ hex y
                else ERROR
        | x ->
            x

    let readEscapedFromChars (chars: int list) =
        let mutable chars = chars
        let read() =
            match chars with
            | [] -> -1
            | h :: t ->
                chars <- t
                h
        let hex x =
            match x with
            | x when x >= int '0' && x <= int '9' -> x - int '0'
            | x when x >= int 'a' && x <= int 'f' -> x - int 'a' + 10
            | x when x >= int 'A' && x <= int 'F' -> x - int 'A' + 10
            | _ -> ERROR
        match read() with
        | x when x = int '~' ->
            match read() with
            | x when x = int 'u' ->
                let a = read()
                let b = read()
                let c = read()
                let d = read()
                if a >= 0 && b >= 0 && c >= 0 && d >= 0 then
                    hex a ++ hex b ++ hex c ++ hex d
                else ERROR
            | x ->
                let y = read()
                if x >= 0 && y >= 0 then
                    hex x ++ hex y
                else ERROR
        | x ->
            x
        , chars

    let read (s: string) = 
        let buf = System.Text.StringBuilder()
        use i = new System.IO.StringReader(s)
        let rec loop () =
            match readEscaped i with
            | ERROR -> None
            | EOF -> Some (string buf)
            | x -> buf.Append(char x) |> ignore; loop ()
        loop ()

type internal PathUtil =
    static member WriteQuery q =
        let sb = StringBuilder 128
        let mutable start = true
        q |> Map.toSeq |> Seq.iter (fun (k: string, v: string) ->
            if start then
                start <- false
            else 
                sb.Append('&') |> ignore                    
            sb.Append(k).Append('=').Append(v) |> ignore
        )
        sb.ToString()

    static member WriteLink s q =
        let sb = StringBuilder 128
        if List.isEmpty s then
            sb.Append('/') |> ignore
        else
            s |> List.iter (fun x ->
                if not (System.String.IsNullOrEmpty x) then
                    sb.Append('/').Append(x) |> ignore
            )
        if Map.isEmpty q then () 
        else 
            let mutable start = true
            sb.Append('?') |> ignore                    
            q |> Map.toSeq |> Seq.iter (fun (k: string, v: string) ->
                if start then
                    start <- false
                else 
                    sb.Append('&') |> ignore                    
                sb.Append(k).Append('=').Append(v) |> ignore
            )
        sb.ToString()

//[<Proxy(typeof<PathUtil>)>]
//type internal PathUtilProxy =
//    static member Concat xs = 
//        let sb = System.Collections.Generic.Queue()
//        let mutable start = true
//        xs |> List.iter (fun x ->
//            if not (System.String.IsNullOrEmpty x) then
//                if start then
//                    start <- false
//                else 
//                    sb.Enqueue("/") |> ignore                    
//                sb.Enqueue(x) |> ignore
//        )
//        sb |> System.String.Concat

//    static member WriteQuery q =
//        q |> Map.toSeq |> Seq.map (fun (k, v) -> k + "=" + v) |> String.concat "&"

//    static member WriteLink s q =
//        let query = 
//            if Map.isEmpty q then "" 
//            else "?" + PathUtil.WriteQuery(q)
//        "/" + PathUtilProxy.Concat s + query

type Route =
    {
        Segments : list<string>
        QueryArgs : Map<string, string>
        FormData : Map<string, string>
        Method : option<string> 
        Body : Lazy<string>
    }

    static member Empty =
        {
            Segments = []
            QueryArgs = Map.empty
            FormData = Map.empty
            Method = None
            Body = Lazy.CreateFromValue null
        }
    
    static member Segment s =
        { Route.Empty with
            Segments = [ s ]
        }

    static member Segment s =
        { Route.Empty with
            Segments = s
        }

    static member Segment (s, m) =
        { Route.Empty with
            Segments = s
            Method = m
        }

    static member Combine (paths: seq<Route>) =
        let paths = Seq.toArray paths
        match paths.Length with
        | 1 -> paths.[0]
        | 0 -> Route.Empty
        | _ ->
        let mutable method = None
        let mutable body = null
        let segments = System.Collections.Generic.Queue()
        let mutable queryArgs = Map.empty
        let mutable formData = Map.empty
        paths |> Array.iter (fun p ->
            match p.Method with
            | Some _ as m ->
                method <- m
            | _ -> ()
            match p.Body.Value with
            | null -> ()
            | b ->
                body <- b
            queryArgs <- p.QueryArgs |> Map.foldBack Map.add queryArgs 
            formData <- p.FormData |> Map.foldBack Map.add formData 
            p.Segments |> List.iter segments.Enqueue
        )
        {
            Segments = List.ofSeq segments
            QueryArgs = queryArgs
            FormData = formData
            Method = method
            Body = Lazy.CreateFromValue body
        }

    static member ParseQuery(q: string) =
        q.Split('&') |> Array.choose (fun kv ->
            match kv.Split('=') with
            | [| k; v |] -> Some (k, v)
            | _ -> 
                printfn "wrong format for query argument: %s" kv
                None
        ) |> Map.ofSeq
    
    static member WriteQuery(q) = PathUtil.WriteQuery q

    static member FromUrl(path: string, ?strict: bool) =
        let s, q = 
            match path.IndexOf '?' with
            | -1 -> path, Map.empty
            | i -> 
                path.Substring(0, i),
                path.Substring(i + 1) |> Route.ParseQuery
        let splitOptions =
            if Option.isSome strict && strict.Value then 
                System.StringSplitOptions.None
            else
                System.StringSplitOptions.RemoveEmptyEntries
        { Route.Empty with
            Segments = 
                s.Split([| '/' |], splitOptions) |> List.ofArray
            QueryArgs = q
        }

    static member FromRequest(r: HttpRequest) =
        let u = r.PathBase.ToUriComponent() |> System.Uri
        let p =
            if u.IsAbsoluteUri then 
                u.AbsolutePath 
            else 
                let s = u.OriginalString
                match s.IndexOf('?') with
                | -1 -> s
                | q -> s.Substring(0, q)
        {
            Segments = p.Split([| '/' |], System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
            QueryArgs = HttpHelpers.CollectionToMap r.Query // HttpTODO
            FormData = HttpHelpers.CollectionToMap r.Form // HttpTODO
            Method = Some (r.Method.ToString())
            Body = lazy r.Body.ToString()
        }

    static member FromHash(path: string, ?strict: bool) =
        match path.IndexOf "#" with
        | -1 -> Route.Empty
        | i -> 
            let h = path.Substring(i + 1)
            if Option.isSome strict && strict.Value then 
                if h = "" || h = "/" then
                    Route.Empty
                elif h.StartsWith "/" then
                    Route.FromUrl(h.Substring(1), true)
                else
                    Route.Segment(h)                    
            else
                Route.FromUrl(path.Substring(i), false)

    member this.ToLink() = PathUtil.WriteLink this.Segments this.QueryArgs

module internal List =
    let rec startsWith s l =
        match s, l with
        | [], _ -> Some l
        | sh :: sr, lh :: lr when sh = lh -> startsWith sr lr
        | _ -> None

type IRouter<'T> =
    abstract Route : HttpRequest -> option<'T>
    abstract Link : 'T -> option<System.Uri>

type Router =
    {
        Parse : Route -> Route seq
        Segment : seq<Route> 
    }
    
    static member FromString (name: string) =
        let parts = name.Split([| '/' |], System.StringSplitOptions.RemoveEmptyEntries)
        if Array.isEmpty parts then 
            {
                Parse = fun path -> Seq.singleton path
                Segment = Seq.empty
            }
        else
            let parts = List.ofArray parts
            {
                Parse = fun path ->
                    match path.Segments |> List.startsWith parts with
                    | Some p -> 
                        Seq.singleton ({ path with Segments = p })
                    | _ -> Seq.empty
                Segment = 
                    Seq.singleton (Route.Segment parts)
            }

    static member (/) (before: Router, after: Router) =
        {
            Parse = fun path ->
                before.Parse path |> Seq.collect after.Parse
            Segment = 
                Seq.append before.Segment after.Segment
        }

    static member (/) (before: string, after: Router) = Router.FromString before / after

    static member (/) (before: Router, after: string) = before / Router.FromString after

    static member (+) (a: Router, b: Router) =
        {
            Parse = fun path ->
                Seq.append (a.Parse path) (b.Parse path) 
            Segment = a.Segment
        }

    static member Combine<'A, 'B when 'A: equality and 'B: equality>(a: Router<'A>, b: Router<'B>) : Router<'A * 'B> =
        a / b

and Router<'T when 'T: equality> =
    {
        Parse : Route -> (Route * 'T) seq
        Write : 'T -> option<seq<Route>> 
    }
    
    static member (/) (before: Router<'T>, after: Router<'U>) =
        {
            Parse = fun path ->
                before.Parse path |> Seq.collect (fun (p, x) -> after.Parse p |> Seq.map (fun (p, y) -> (p, (x, y))))
            Write = fun (v1, v2) ->
                match before.Write v1, after.Write v2 with
                | Some p1, Some p2 -> Some (Seq.append p1 p2)
                | _ -> None
        }

    static member (/) (before: Router, after: Router<'T>) =
        {
            Parse = fun path ->
                before.Parse path |> Seq.collect after.Parse
            Write = fun v ->
                after.Write v |> Option.map (Seq.append before.Segment)
        }

    static member (/) (before: Router<'T>, after: Router) =
        {
            Parse = fun path ->
                before.Parse path |> Seq.collect (fun (p, x) -> after.Parse p |> Seq.map (fun p -> (p, x)))
            Write = fun v ->
                before.Write v |> Option.map (fun x -> Seq.append x after.Segment)
        }

    static member (/) (before: string, after: Router<'T>) = Router.FromString before / after

    static member (/) (before: Router<'T>, after: string) = before / Router.FromString after

    static member (+) (a: Router<'T>, b: Router<'T>) =
        {
            Parse = fun path ->
                Seq.append (a.Parse path) (b.Parse path) 
            Write = fun value ->
                match a.Write value with
                | None -> b.Write value
                | p -> p
        }

    interface IRouter<'T> with
        member this.Route req = 
            let path = Route.FromRequest req
            this.Parse path
            |> Seq.tryPick (fun (path, value) -> if List.isEmpty path.Segments then Some value else None)
        member this.Link ep =
            this.Write ep |> Option.map (fun p -> System.Uri((Route.Combine p).ToLink(), System.UriKind.Relative))
        
module Router =
    let Combine (a: Router<'A>) (b: Router<'B>) = a / b

    let Shift (prefix: string) (router: Router<'A>) =
        prefix / router

    let Empty<'A when 'A: equality> : Router<'A> =
        {
            Parse = fun _ -> Seq.empty
            Write = fun _ -> None
        }

    /// Creates a fully customized router.
    let New (route: HttpRequest -> option<'T>) (link: 'T -> option<System.Uri>) =
        { new IRouter<'T> with
            member this.Route req = route req
            member this.Link e = link e
        }

    /// Creates a router for parsing/writing a full route using URL segments.
    let Create (ser: 'T -> list<string>) (des: list<string> -> option<'T>) =
        {
            Parse = fun path ->
                match des path.Segments with
                | Some ep ->
                    Seq.singleton ({ path with Segments = [] }, ep)
                | None ->
                    Seq.empty
            Write = fun value ->
                Some (Seq.singleton (Route.Segment(ser value)))
        } : Router<'T>

    /// Creates a router for parsing/writing a full route using URL segments and query parameters.
    let CreateWithQuery (ser: 'T -> list<string> * Map<string, string>) (des: list<string> * Map<string, string> -> option<'T>) =
        {
            Parse = fun path ->
                match des (path.Segments, path.QueryArgs) with
                | Some ep ->
                    Seq.singleton ({ path with Segments = [] }, ep)
                | None ->
                    Seq.empty
            Write = fun value ->
                let s, q = ser value
                Some (Seq.singleton { Route.Empty with Segments = s; QueryArgs = q })
        }
    
    /// Parses/writes a single value from a query argument with the given key instead of url path.
    let Query key (item: Router<'A>) : Router<'A> =
        {
            Parse = fun path ->
                match path.QueryArgs.TryFind key with
                | None -> Seq.empty
                | Some q -> 
                    let newQa = path.QueryArgs |> Map.remove key
                    item.Parse { Route.Empty with Segments = [ q ] }
                    |> Seq.map (fun (p, v) ->
                        { path with QueryArgs = newQa }, v
                    )
            Write = fun value ->
                item.Write value |> Option.map (fun p -> 
                    let p = Route.Combine p
                    match p.Segments with
                    | [ v ] -> Seq.singleton { Route.Empty with QueryArgs = Map.ofList [ key, v ] }
                    | _ -> Seq.empty
                )
        }

    /// Parses/writes a single option value from an optional query argument with the given key instead of url path.
    let QueryOption key (item: Router<'A>) : Router<option<'A>> =
        {
            Parse = fun path ->
                match path.QueryArgs.TryFind key with
                | None -> Seq.singleton (path, None)
                | Some q -> 
                    let newQa = path.QueryArgs |> Map.remove key
                    item.Parse { Route.Empty with Segments = [ q ] }
                    |> Seq.map (fun (_, v) ->
                        { path with QueryArgs = newQa }, Some v
                    )
            Write = fun value ->
                match value with
                | None -> Some Seq.empty
                | Some v ->
                    item.Write v |> Option.map (fun p -> 
                        let p = Route.Combine p
                        match p.Segments with
                        | [ v ] -> Seq.singleton { Route.Empty with QueryArgs = Map.ofList [ key, v ] }
                        | _ -> Seq.empty
                    )
        }

    /// Parses/writes a single nullable value from an optional query argument with the given key instead of url path.
    let QueryNullable key (item: Router<'A>) : Router<System.Nullable<'A>> =
        {
            Parse = fun path ->
                match path.QueryArgs.TryFind key with
                | None -> Seq.singleton (path, System.Nullable())
                | Some q -> 
                    let newQa = path.QueryArgs |> Map.remove key
                    item.Parse { Route.Empty with Segments = [ q ] }
                    |> Seq.map (fun (_, v) ->
                        { path with QueryArgs = newQa }, System.Nullable v
                    )
            Write = fun value ->
                if value.HasValue then
                    item.Write value.Value |> Option.map (fun p -> 
                        let p = Route.Combine p
                        match p.Segments with
                        | [ v ] -> Seq.singleton { Route.Empty with QueryArgs = Map.ofList [ key, v ] }
                        | _ -> Seq.empty
                    )
                else
                    Some Seq.empty
        }

    let Method (m: string) : Router =
        {
            Parse = fun path ->
                match path.Method with
                | Some pm when pm = m -> Seq.singleton path
                | _ -> Seq.empty
            Segment =
                Seq.singleton { Route.Empty with Method = Some m }
        }

    [<System.Obsolete("Do not use request body for routing.")>]
    let Body (deserialize: string -> option<'A>) (serialize: 'A -> string) : Router<'A> =
        {
            Parse = fun path ->
                match path.Body.Value with
                | null -> Seq.empty
                | x ->
                    match deserialize x with
                    | Some b -> Seq.singleton ({ path with Body = Lazy.CreateFromValue null}, b)
                    | _ -> Seq.empty
            Write = fun value ->
                Some <| Seq.singleton { Route.Empty with Body = Lazy.CreateFromValue (serialize value) }
        }

    let FormData (item: Router<'A>) : Router<'A> =
        {
            Parse = fun path ->
                item.Parse { path with QueryArgs = path.FormData }
                |> Seq.map (fun (_, r) -> path, r)
            Write = fun value ->
                item.Write value
                |> Option.map (Seq.map (fun p -> { p with QueryArgs = Map.empty; FormData = p.QueryArgs }))  
        }
    
    let Parse (router: Router<'A>) path =
        router.Parse path
        |> Seq.tryPick (fun (path, value) -> if List.isEmpty path.Segments then Some value else None)

    let Write (router: Router<'A>) endpoint =
        router.Write endpoint |> Option.map Route.Combine 

    let TryLink (router: Router<'A>) endpoint =
        match Write router endpoint with
        | Some p -> Some (p.ToLink())
        | None -> None

    let Link (router: Router<'A>) endpoint =
        match Write router endpoint with
        | Some p -> p.ToLink()
        | None -> ""

    let HashLink (router: Router<'A>)  endpoint =
        "#" + Link router endpoint
    
    /// Maps a router to a narrower router type. The decode function must return None if the
    /// value can't be mapped to a value of the target.
    let Slice (decode: 'T -> 'U option) (encode: 'U -> 'T) (router: Router<'T>) : Router<'U> =
        {
            Parse = fun path ->
                router.Parse path |> Seq.choose (fun (p, v) -> decode v |> Option.map (fun v -> p, v)) 
            Write = fun value ->
                encode value |> router.Write
        }

    /// Maps a router to a wider router type. The encode function must return None if the
    /// value can't be mapped back to a value of the source.
    let Embed (decode: 'A -> 'B) (encode: 'B -> 'A option) router =
        {
            Parse = fun path ->
                router.Parse path |> Seq.map (fun (p, v) -> p, decode v) 
            Write = fun value ->
                encode value |> Option.bind router.Write
        }

    /// Maps a router with a bijection.
    let Map (decode: 'A -> 'B) (encode: 'B -> 'A) router =
        {
            Parse = fun path ->
                router.Parse path |> Seq.map (fun (p, v) -> p, decode v) 
            Write = fun value ->
                encode value |> router.Write
        }

    /// Combination of Slice and Embed, a mapping from a subset of source values to
    /// a subset of target values. Both encode and decode must return None if
    /// there is no mapping to a value of the other type.
    let TryMap (decode: 'A -> 'B option) (encode: 'B -> 'A option) router =
        {
            Parse = fun path ->
                router.Parse path |> Seq.choose (fun (p, v) -> decode v |> Option.map (fun v -> p, v)) 
            Write = fun value ->
                encode value |> Option.bind router.Write
        }

    /// Filters a router, only parsing/writing values that pass the predicate check.
    let Filter predicate router =
        {
            Parse = fun path ->
                router.Parse path |> Seq.filter (snd >> predicate)
            Write = fun value ->
                if predicate value then router.Write value else None
        }

    let private BoxImpl tryUnbox (router: Router<'A>): Router<obj> =
        {
            Parse = fun path ->
                router.Parse path |> Seq.map (fun (p, v) -> p, box v) 
            Write = fun value ->
                tryUnbox value |> Option.bind router.Write
        }

    /// Converts to Router<obj>. When writing, a type check against type A is performed.
    let Box (router: Router<'A>): Router<obj> =
        BoxImpl (function :? 'A as v -> Some v | _ -> None) router

    [<System.Obsolete("Do not use request body for routing.")>]
    let Json<'T when 'T: equality> : Router<'T> =
        Body (fun s -> try Some (Json.JsonSerializer.Deserialize<'T> s) with _ -> None) Json.JsonSerializer.Serialize<'T>

    let UnboxImpl<'A when 'A: equality> tryUnbox (router: Router<obj>) : Router<'A> =
        {
            Parse = fun path ->
                router.Parse path |> Seq.choose (fun (p, v) -> match tryUnbox v with Some v -> Some (p, v) | _ -> None) 
            Write = fun value ->
                box value |> router.Write
        }

    /// Converts from Router<obj>. When parsing, a type check against type A is performed.
    let Unbox<'A when 'A: equality> (router: Router<obj>) : Router<'A> =
        UnboxImpl (function :? 'A as v -> Some v | _ -> None) router

    let private CastImpl tryParseCast tryWriteCast (router: Router<'A>): Router<'B> =
        {
            Parse = fun path ->
                router.Parse path |> Seq.choose (fun (p, v) -> match tryParseCast v with Some v -> Some (p, v) | _ -> None) 
            Write = fun value ->
                tryWriteCast value |> Option.bind router.Write
        }

    /// Converts a Router<A> to Router<B>. When parsing and writing, type checks are performed.
    /// Upcasting do not change set of parsed routes, downcasting restricts it within the target type.
    let Cast (router: Router<'A>): Router<'B> =
        CastImpl (fun v -> match box v with :? 'B as v -> Some v | _ -> None) (fun v -> match box v with :? 'A as v -> Some v | _ -> None) router

    /// Maps a single-valued (non-generic) Router to a specific value.
    let MapTo value (router: Router) =
        {
            Parse = fun path ->
                router.Parse path |> Seq.map (fun p -> p, value) 
            Write = fun v ->
                if v = value then Some router.Segment else None
        }

    /// Parses/writes using any of the routers, attempts are made in the given order.
    let Sum (routers: seq<Router<_>>) =
        let routers = Array.ofSeq routers
        {
            Parse = fun path ->
                routers |> Seq.collect (fun r -> r.Parse path)
            Write = fun value ->
                routers |> Seq.tryPick (fun r -> r.Write value)
        }
    
    // todo: optimize
    let Table<'T when 'T : equality> (mapping: seq<'T * string>) : Router<'T> =
        mapping |> Seq.map (fun (v, s) -> Router.FromString s |> MapTo v) |> Sum 

    let Single<'T when 'T : equality> (endpoint: 'T) (route: string) : Router<'T> =
        let parts = route.Split([| '/' |], System.StringSplitOptions.RemoveEmptyEntries)
        if Array.isEmpty parts then 
            {
                Parse = fun path -> Seq.singleton (path, endpoint)
                Write = fun value -> if value = endpoint then Some Seq.empty else None
            }
        else
            let parts = List.ofArray parts
            {
                Parse = fun path ->
                    match path.Segments |> List.startsWith parts with
                    | Some p -> 
                        Seq.singleton ({ path with Segments = p }, endpoint)
                    | _ -> Seq.empty
                Write = fun value ->
                    if value = endpoint then Some (Seq.singleton (Route.Segment parts)) else None
            }

    let Delay<'T when 'T: equality> (getRouter: unit -> Router<'T>) : Router<'T> =
        let r = lazy getRouter()
        {
            Parse = fun path -> r.Value.Parse path
            Write = fun value -> r.Value.Write value
        }

    /// Creates a router for parsing/writing an Array of values.
    let Array (item: Router<'A>) : Router<'A[]> =
        {
            Parse = fun path ->
                match path.Segments with
                | h :: t -> 
                    match System.Int32.TryParse h with
                    | true, l ->
                        let rec collect l path acc =
                            if l = 0 then Seq.singleton (path, Array.ofList (List.rev acc))
                            else item.Parse path |> Seq.collect(fun (p, a) -> collect (l - 1) p (a :: acc))
                        collect l { path with Segments = t } []
                    | _ -> Seq.empty
                | _ -> Seq.empty
            Write = fun value ->
                let parts = value |> Array.map item.Write
                if Array.forall Option.isSome parts then
                    Some (Seq.append (Seq.singleton (Route.Segment (string value.Length))) (parts |> Seq.collect Option.get))
                else None                      
        }

    /// Creates a router for parsing/writing a Nullable value.
    let Nullable (item: Router<'A>) : Router<System.Nullable<'A>> =
        {
            Parse = fun path ->
                match path.Segments with
                | "null" :: p -> 
                    Seq.singleton ({ path with Segments = p }, System.Nullable())
                | _ ->
                    item.Parse path |> Seq.map (fun (p, v) -> p, System.Nullable v)
            Write = fun value ->
                if value.HasValue then 
                    item.Write value.Value
                else 
                    Some (Seq.singleton (Route.Segment "null"))
        }

    /// Creates a router for parsing/writing an F# option of a value.
    let Option (item: Router<'A>) : Router<'A option> =
        {
            Parse = fun path ->
                match path.Segments with
                | "None" :: p -> 
                    Seq.singleton ({ path with Segments = p }, None)
                | "Some" :: p ->
                    item.Parse { path with Segments = p } |> Seq.map (fun (p, v) -> p, Some v)
                | _ ->
                    Seq.empty
            Write = fun value ->
                match value with 
                | None -> Some (Seq.singleton (Route.Segment "None"))
                | Some v -> 
                    item.Write v |> Option.map (Seq.append (Seq.singleton (Route.Segment "Some")))
        }

    module FArray = Collections.Array

    type IListArrayConverter =
        abstract OfArray: obj -> obj
        abstract ToArray: obj -> obj

    type ListArrayConverter<'T>() =
        interface IListArrayConverter with
            member this.OfArray a = List.ofArray (unbox<'T []> a) |> box
            member this.ToArray l = List.toArray (unbox<'T list> l) |> box

    /// Creates a router for parsing/writing an F# list of a value.
    let List (item: Router<'A>) : Router<'A list> =
        Array item |> Map List.ofArray FArray.ofList

type Router with
    member this.MapTo(value: 'T) =
        Router.MapTo value this

    static member Sum ([<System.ParamArray>] routers: Router<'T>[]) =
        Router.Sum routers

    static member Empty<'T when 'T: equality>() =
        Router.Empty<'T>

    static member New(route: System.Func<HttpRequest, 'T>, link: System.Func<'T, System.Uri>) =
        Router.New (route.Invoke >> Option.ofObj) (link.Invoke >> Option.ofObj)

    static member Method(method:string) =
        Router.Method method

    [<System.Obsolete("Do not use request body for routing.")>]
    static member Body(des:System.Func<string, 'T>, ser: System.Func<'T, string>) =
        Router.Body (fun s -> des.Invoke s |> Option.ofObj) ser.Invoke 

    [<System.Obsolete("Do not use request body for routing.")>]
    static member Json<'T when 'T: equality>() =
        Router.Json<'T>

    static member Table([<System.ParamArray>] mapping: ('T * string)[]) =
        Router.Table mapping

    static member Single(endpoint, route) =
        Router.Single endpoint route

    static member Delay(getRouter: System.Func<Router<'T>>) =
        Router.Delay getRouter.Invoke

type Router<'T when 'T: equality> with

    member this.Query(key: string) =
        Router.Query key this

    member this.Link(endpoint: 'T) =
        Router.Link this endpoint

    member this.TryLink(endpoint: 'T, link: byref<string>) =
        match Router.TryLink this endpoint with
        | Some l ->
            link <- l
            true
        | _ -> false
               
    member this.HashLink(endpoint: 'T) =
        Router.HashLink this endpoint

    member this.Map(decode: System.Func<'T, 'U>, encode: System.Func<'U, 'T>) =
        Router.TryMap (decode.Invoke >> ofObjNoConstraint) (encode.Invoke >> ofObjNoConstraint) this

    member this.Filter(predicate: System.Func<'T, bool>) =
        Router.Filter predicate.Invoke this

    member this.Cast<'U when 'U: equality>() : Router<'U> =
        Router.Cast this

    member this.FormData() =
        Router.FormData this

    member this.Box() =
        Router.Box this

    member this.Array() =
        Router.Array this

[<Extension>]
type RouterExtensions =
    static member QueryNullable(router, key) =
        Router.QueryNullable key router

    static member Unbox<'T when 'T: equality>(router) =
        Router.Unbox<'T> router

    static member Nullable(router) =
        Router.Nullable router

module IRouter =
    open System

    let Empty : IRouter<'T> =
        { new IRouter<'T> with
            member this.Route _ = None
            member this.Link _ = None
        }        

    let Add (r1: IRouter<'T>) (r2: IRouter<'T>) =
        { new IRouter<'T> with
            member this.Route req = match r1.Route req with Some _ as l -> l | _ -> r2.Route req
            member this.Link e = match r1.Link e with Some _ as l -> l | _ -> r2.Link e
        }        

    let Sum (routers: seq<IRouter<'T>>) : IRouter<'T> =
        let routers = Array.ofSeq routers
        if Seq.isEmpty routers then Empty else
            { new IRouter<'T> with
                member this.Route req = routers |> Array.tryPick (fun r -> r.Route req)
                member this.Link e = routers |> Array.tryPick (fun r -> r.Link e)
            }        
            
    let Map encode decode (router: IRouter<'T>) : IRouter<'U> =
        { new IRouter<'U> with
            member this.Route req = router.Route req |> Option.map encode
            member this.Link e = decode e |> router.Link
        } 
        
    let TryMap encode decode (router: IRouter<'T>) : IRouter<'U> =
        { new IRouter<'U> with
            member this.Route req = router.Route req |> Option.bind encode
            member this.Link e = decode e |> Option.bind router.Link
        } 

    let Embed encode decode (router: IRouter<'T>) : IRouter<'U> =
        { new IRouter<'U> with
            member this.Route req = router.Route req |> Option.map encode
            member this.Link e = decode e |> Option.bind router.Link
        } 

    let private makeUri uri =
        let mutable res = null
        if Uri.TryCreate(uri, UriKind.Relative, &res) then res else
            Uri(uri, UriKind.Absolute)
    
    let private path (uri: Uri) =
        if uri.IsAbsoluteUri
        then uri.AbsolutePath
        else uri.OriginalString |> joinWithSlash "/"
        
    let private trimFinalSlash (s: string) =
        match s.TrimEnd('/') with
        | "" -> "/"
        | s -> s
    
    let Shift prefix (router: IRouter<'T>) =
        let prefix = joinWithSlash "/" prefix
        let shift (loc: System.Uri) =
            if loc.IsAbsoluteUri then loc else
                makeUri (joinWithSlash prefix (path loc) |> trimFinalSlash)
        { new IRouter<'T> with
            member this.Route req =
                let builder = req.PathBase.ToUriComponent() |> System.Uri |> UriBuilder
                if builder.Path.StartsWith prefix then
                    builder.Path <- builder.Path.Substring prefix.Length
                    req.PathBase <- builder.Uri |> PathString.FromUriComponent
                    router.Route req
                else
                    None
            member this.Link e = router.Link e |> Option.map shift
        }     
        
    let Box (router: IRouter<'T>) : IRouter<obj> =
        { new IRouter<obj> with
            member this.Route req = router.Route req |> Option.map box
            member this.Link e = tryUnbox<'T> e |> Option.bind router.Link
        } 

    let Unbox (router: IRouter<obj>) : IRouter<'T> =
        { new IRouter<'T> with
            member this.Route req = router.Route req |> Option.bind tryUnbox<'T>
            member this.Link e = box e |> router.Link
        } 

module RouterOperators =
    open System.Globalization
    
    let rRoot : Router =
        {
            Parse = fun path -> Seq.singleton path
            Segment = Seq.empty
        }
    
    /// Parse/write a specific string.
    let r name : Router = Router.FromString name

    /// Parse/write a string using URL encode/decode.
    let rString : Router<string> =
        {
            Parse = fun path ->
                match path.Segments with
                | h :: t -> 
                    match StringEncoding.read h with
                    | Some s ->
                        Seq.singleton ({ path with Segments = t }, s)
                    | _ -> Seq.empty
                | [] ->
                    Seq.singleton (path, "")
            Write = fun value ->
                Some (Seq.singleton (Route.Segment (if isNull value then "null" else StringEncoding.write value)))
        }

    /// Parse/write a char.
    let rChar : Router<char> =
        {
            Parse = fun path ->
                match path.Segments with
                | h :: t -> 
                    match StringEncoding.read h with
                    | Some c when c.Length = 1 ->
                        Seq.singleton ({ path with Segments = t }, char c)
                    | _ -> Seq.empty
                | _ -> Seq.empty
            Write = fun value ->
                Some (Seq.singleton (Route.Segment (string value)))
        }

    let inline rTryParse< ^T when ^T: (static member TryParse: string * byref< ^T> -> bool) and ^T: equality>() =
        {
            Parse = fun path ->
                match path.Segments with
                | h :: t -> 
                    let mutable res = Unchecked.defaultof< ^T>
                    let ok = (^T: (static member TryParse: string * byref< ^T> -> bool) (h, &res))
                    if ok then 
                        Seq.singleton ({ path with Segments = t }, res)
                    else Seq.empty
                | _ -> Seq.empty
            Write = fun value ->
                Some (Seq.singleton (Route.Segment (string value)))
        }

    let inline rTryParseFloat< ^T when ^T: (static member TryParse: string * NumberStyles * NumberFormatInfo * byref< ^T> -> bool) and ^T: equality>() =
        {
            Parse = fun path ->
                match path.Segments with
                | h :: t -> 
                    let mutable res = Unchecked.defaultof< ^T>
                    let ok =
                        (^T: (static member TryParse: string * NumberStyles * NumberFormatInfo * byref< ^T> -> bool)
                            (h, NumberStyles.Float, NumberFormatInfo.InvariantInfo, &res))
                    if ok then 
                        Seq.singleton ({ path with Segments = t }, res)
                    else Seq.empty
                | _ -> Seq.empty
            Write = fun value ->
                Some (Seq.singleton (Route.Segment (string value)))
        }

    /// Parse/write a Guid.
    let rGuid = rTryParse<System.Guid>()
    /// Parse/write an int.
    let rInt = rTryParse<int>()
    /// Parse/write a double.
    let rDouble = rTryParseFloat<double>()
    /// Parse/write a signed byte.
    let rSByte = rTryParse<sbyte>() 
    /// Parse/write a byte.
    let rByte = rTryParse<byte>() 
    /// Parse/write a 16-bit int.
    let rInt16 = rTryParse<int16>() 
    /// Parse/write a 16-bit unsigned int.
    let rUInt16 = rTryParse<uint16>() 
    /// Parse/write an unsigned int.
    let rUInt = rTryParse<uint32>() 
    /// Parse/write a 64-bit int.
    let rInt64 = rTryParse<int64>() 
    /// Parse/write a 64-bit unsigned int.
    let rUInt64 = rTryParse<uint64>() 
    /// Parse/write a single.
    let rSingle = rTryParseFloat<single>()

    /// Parse/write a bool.
    let rBool : Router<bool> =
        // we define rBool not with rTryParse so that fragments are capitalized
        // to be fully consistent on client+server
        {
            Parse = fun path ->
                match path.Segments with
                | h :: t -> 
                    match System.Boolean.TryParse h with
                    | true, g ->
                        Seq.singleton ({ path with Segments = t }, g)
                    | _ -> Seq.empty
                | _ -> Seq.empty
            Write = fun value ->
                Some (Seq.singleton (Route.Segment (if value then "True" else "False")))
        }

    /// Parses any remaining part of the URL as a string, no URL encode/decode is done.
    let rWildcard : Router<string> = 
        {
            Parse = fun path ->
                let s = path.Segments |> String.concat "/"
                Seq.singleton ({ path with Segments = [] }, s)
            Write = fun value ->
                Some (Seq.singleton (Route.Segment value))
        }
    
    let rWildcardArray (item: Router<'A>) : Router<'A[]> =
        {
            Parse = fun path ->
                let rec collect path acc =
                    match path.Segments with
                    | [] -> Seq.singleton (path, Array.ofList (List.rev acc))
                    | _ ->
                        item.Parse path |> Seq.collect(fun (p, a) -> collect p (a :: acc))
                collect path []
            Write = fun value ->
                let parts = value |> Array.map item.Write
                if Array.forall Option.isSome parts then
                    Some (parts |> Seq.collect Option.get)
                else None                      
        }

    let rWildcardList (item: Router<'A>) : Router<'A list> = 
        {
            Parse = fun path ->
                let rec collect path acc =
                    match path.Segments with
                    | [] -> Seq.singleton (path, List.rev acc)
                    | _ ->
                        item.Parse path |> Seq.collect(fun (p, a) -> collect p (a :: acc))
                collect path []
            Write = fun value ->
                let parts = value |> List.map item.Write
                if List.forall Option.isSome parts then
                    Some (parts |> Seq.collect Option.get)
                else None                      
        }

    /// Parse/write a DateTime in `YYYY-MM-DD-HH.mm.ss` format.
    let rDateTime : Router<System.DateTime> =
        let pInt (x: string) =
            match System.Int32.TryParse x with
            | true, i -> Some i
            | _ -> None
        {
            Parse = fun path ->
                match path.Segments with
                | h :: t -> 
                    if h.Length = 19 && h.[4] = '-' && h.[7] = '-' && h.[10] = '-' && h.[13] = '.' && h.[16] = '.' then
                        match pInt h.[0 .. 3], pInt h.[5 .. 6], pInt h.[8 .. 9], pInt h.[11 .. 12], pInt h.[14 .. 15], pInt h.[17 .. 18] with
                        | Some y, Some m, Some d, Some h, Some mi, Some s  ->
                            Seq.singleton ({ path with Segments = t }, System.DateTime(y, m, d, h, mi, s))
                        | _ -> Seq.empty
                    else Seq.empty
                | _ -> Seq.empty
            Write = fun d ->
                let pad2 (x: int) =
                    let s = string x
                    if s.Length = 1 then "0" + s else s
                let pad4 (x: int) =
                    let s = string x
                    match s.Length with
                    | 1 -> "000" + s
                    | 2 -> "00" + s
                    | 3 -> "0" + s
                    | _ -> s
                let s = 
                    pad4 d.Year + "-" + pad2 d.Month + "-" + pad2 d.Day
                    + "-" + pad2 d.Hour + "." + pad2 d.Minute + "." + pad2 d.Second
                Some (Seq.singleton (Route.Segment s))
        }

    let rCors<'T when 'T : equality> (r: Router<'T>) : Router<Cors<'T>> =
        {
            Parse = fun path ->
                r.Parse path
                |> Seq.map (fun (p, e) -> (p, Cors.Of e))
            Write = function
                | Cors.Of e -> r.Write e
                | Cors.Preflight -> Some (Seq.singleton Route.Empty)
        }
      
    let internal Tuple (readItems: obj -> obj[]) (createTuple: obj[] -> obj) (items: Router<obj>[]) =
        {
            Parse = fun path ->
                let rec collect elems path acc =
                    match elems with 
                    | [] -> Seq.singleton (path, createTuple (Array.ofList (List.rev acc)))
                    | h :: t -> h.Parse path |> Seq.collect(fun (p, a) -> collect t p (a :: acc))
                collect (List.ofArray items) path []
            Write = fun value ->
                let parts =
                    (readItems value, items) ||> Array.map2 (fun v r ->
                        r.Write v
                    )
                if Array.forall Option.isSome parts then
                    Some (parts |> Seq.collect Option.get)
                else None                      
        }