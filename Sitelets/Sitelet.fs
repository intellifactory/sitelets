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

open Microsoft.AspNetCore.Mvc

#nowarn "44" // Obsolete CustomContent, CustomContentAsync, PageContent, PageContentAsync

open System
open System.Threading.Tasks
open System.Runtime.CompilerServices
open Microsoft.AspNetCore.Http

type Context<'T> =
    {
        Link : 'T -> string
        HttpContext : HttpContext
    }

module Context =
    let Map (f: 'T2 -> 'T1) (ctx: Context<'T1>) : Context<'T2> =
        {
            Link = (ctx.Link << f)
            HttpContext = ctx.HttpContext
        }

type Sitelet<'T when 'T : equality> =
    {
        Router : IRouter<'T>
        Controller : Context<'T> -> 'T -> obj
    }

    static member (+) (s1: Sitelet<'T>, s2: Sitelet<'T>) =
        {
            Router = IRouter.Add s1.Router s2.Router
            Controller = fun ctx endpoint ->
                match s1.Router.Link endpoint with
                | Some _ -> s1.Controller ctx endpoint
                | None -> s2.Controller ctx endpoint
        }

    static member ( <|> ) (s1: Sitelet<'T>, s2: Sitelet<'T>) = s1 + s2

/// Provides combinators over sitelets.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Sitelet =
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Reflection

    /// Creates an empty sitelet.
    let Empty<'T when 'T : equality> : Sitelet<'T> =
        {
            Router = Router.Empty<'T>
            Controller = fun ctx endpoint -> null
        }

    /// Creates a WebSharper.Sitelet using the given router and handler function.
    let New (router: IRouter<'T>) (handle: Context<'T> -> 'T -> obj) =
        {
            Router = router
            Controller = handle
        }

    /// Represents filters for protecting sitelets.
    type Filter<'T> =
        {
            VerifyUser : string -> bool;
            LoginRedirect : 'T -> 'T
        }

    /// Constructs a protected sitelet given the filter specification.
    let Protect (filter: Filter<'T>) (site: Sitelet<'T>)
        : Sitelet<'T> =
        {
            Router = site.Router
            Controller = fun ctx endpoint ->
                let prot = filter
                let failure () =
                    RedirectResult (ctx.Link endpoint) |> box
                let loggedIn = 
                    let nameClaim = ctx.HttpContext.User.FindFirst(Security.Claims.ClaimTypes.NameIdentifier)
                    if isNull nameClaim then None else Some nameClaim.Value
                match loggedIn with
                | Some user ->
                    if prot.VerifyUser user then
                        site.Controller ctx endpoint
                    else
                        failure ()
                | None ->
                    failure ()
        }

    /// Constructs a singleton sitelet that contains exactly one endpoint
    /// and serves a single content value at a given location.
    let Content (location: string) (endpoint: 'T) (cnt: Context<'T> -> obj) =
        {
            Router = Router.Single endpoint location
            Controller = fun ctx _ -> cnt ctx 
        }

    /// Maps over the sitelet endpoint type. Requires a bijection.
    let Map (f: 'T1 -> 'T2) (g: 'T2 -> 'T1) (s: Sitelet<'T1>) : Sitelet<'T2> =
        {
            Router = IRouter.Map f g s.Router
            Controller = fun ctx endpoint ->
                s.Controller (Context.Map f ctx) (g endpoint)
        }

    /// Maps over the served sitelet content.
    let MapContent (f: obj -> obj) (sitelet: Sitelet<'T>) : Sitelet<'T> =
        { sitelet with
            Controller = fun ctx ep ->
                ep
                |> sitelet.Controller ctx
                |> f
        }

    /// Maps over the sitelet endpoint type. Requires a bijection.
    let TryMap (f: 'T1 -> 'T2 option) (g: 'T2 -> 'T1 option) (s: Sitelet<'T1>) : Sitelet<'T2> =
        {
            Router = IRouter.TryMap f g s.Router
            Controller = fun ctx a ->
                match g a with
                | Some ea -> s.Controller (Context.Map (f >> Option.get) ctx) ea
                | None -> failwith "Invalid endpoint in Sitelet.Embed"
        }

    /// Maps over the sitelet endpoint type with only an injection.
    let Embed embed unembed sitelet =
        {
            Router = IRouter.Embed embed unembed sitelet.Router
            Controller = fun ctx a ->
                match unembed a with
                | Some ea -> sitelet.Controller (Context.Map embed ctx) ea
                | None -> failwith "Invalid endpoint in Sitelet.Embed"
        }

    let tryGetEmbedFunctionsFromExpr (expr: Expr<'T1 -> 'T2>) =
        match expr with
        | ExprShape.ShapeLambda(_, Patterns.NewUnionCase (uci, _)) ->
            let embed (y: 'T1) = FSharpValue.MakeUnion(uci, [|box y|]) :?> 'T2
            let unembed (x: 'T2) =
                let uci', args' = FSharpValue.GetUnionFields(box x, uci.DeclaringType)
                if uci.Tag = uci'.Tag then
                    Some (args'.[0] :?> 'T1)
                else None
            Some (embed, unembed)
        | _ -> None
 
    /// Maps over the sitelet endpoint type, where the destination type
    /// is a discriminated union with a case containing the source type.
    let EmbedInUnion (case: Expr<'T1 -> 'T2>) sitelet =
        match tryGetEmbedFunctionsFromExpr case with
        | Some (embed, unembed) -> Embed embed unembed sitelet
        | None -> failwith "Invalid union case in Sitelet.EmbedInUnion"

    /// Shifts all sitelet locations by a given prefix.
    let Shift (prefix: string) (sitelet: Sitelet<'T>) =
        {
            Router = IRouter.Shift prefix sitelet.Router
            Controller = sitelet.Controller
        }

    /// Combines several sitelets, leftmost taking precedence.
    /// Is equivalent to folding with the choice operator.
    let Sum (sitelets: seq<Sitelet<'T>>) : Sitelet<'T> =
        let sitelets = Array.ofSeq sitelets 
        if Seq.isEmpty sitelets then Empty else
            {
                Router = IRouter.Sum (sitelets |> Seq.map (fun s -> s.Router))
                Controller = fun ctx endpoint ->
                    sitelets 
                    |> Array.pick (fun s -> 
                        match s.Router.Link endpoint with
                        | Some _ -> Some (s.Controller ctx endpoint)
                        | None -> None
                    )
            }

    /// Serves the sum of the given sitelets under a given prefix.
    /// This function is convenient for folder-like structures.
    let Folder<'T when 'T : equality> (prefix: string)
                                      (sitelets: seq<Sitelet<'T>>) =
        Shift prefix (Sum sitelets)

    /// Boxes the sitelet endpoint type to Object type.
    let Box (sitelet: Sitelet<'T>) : Sitelet<obj> =
        {
            Router = IRouter.Box sitelet.Router
            Controller = fun ctx a ->
                sitelet.Controller (Context.Map box ctx) (unbox a)
        }

    let Upcast sitelet = Box sitelet

    /// Reverses the Box operation on the sitelet.
    let Unbox<'T when 'T : equality> (sitelet: Sitelet<obj>) : Sitelet<'T> =
        {
            Router = IRouter.Unbox sitelet.Router
            Controller = fun ctx a ->
                sitelet.Controller (Context.Map unbox ctx) (box a)
        }

    let UnsafeDowncast sitelet = Unbox sitelet

    /// Constructs a sitelet with an inferred router and a given controller
    /// function.
    let Infer<'T when 'T : equality> (handle : Context<'T> -> 'T -> obj) =
        {
            Router = Router.IInfer<'T>()
            Controller = handle
        }

    let InferWithCustomErrors<'T when 'T : equality> (handle : Context<'T> -> ParseRequestResult<'T> -> obj) =
        {
            Router = Router.IInferWithCustomErrors<'T>()
            Controller = fun ctx x ->
                handle (Context.Map ParseRequestResult.Success ctx) x
        }

    let InferPartial (embed: 'T1 -> 'T2) (unembed: 'T2 -> 'T1 option) (mkContent: Context<'T2> -> 'T1 -> obj) : Sitelet<'T2> =
        {
            Router = Router.IInfer<'T1>() |> IRouter.Embed embed unembed
            Controller = fun ctx p ->
                    match unembed p with
                    | Some e ->
                        mkContent ctx e
                    | None ->
                        failwith "Invalid endpoint in Sitelet.InferPartial"
        }

    let InferPartialInUnion (case: Expr<'T1 -> 'T2>) mkContent =
        match tryGetEmbedFunctionsFromExpr case with
        | Some (embed, unembed) -> InferPartial embed unembed mkContent
        | None -> failwith "Invalid union case in Sitelet.InferPartialInUnion"

    let MapContext (f: Context<'T> -> Context<'T>) (sitelet: Sitelet<'T>) : Sitelet<'T> =
        { sitelet with
            Controller = fun ctx action ->
                sitelet.Controller (f ctx) action
        }

    // let WithSettings (settings: seq<string * string>) (sitelet: Sitelet<'T>) : Sitelet<'T> =
    //     MapContext (Context.WithSettings settings) sitelet // TODO

type RouteHandler<'T> = delegate of Context<obj> * 'T -> obj

[<CompiledName "Sitelet"; Sealed>]
type CSharpSitelet =
        
    static member Empty = Sitelet.Empty<obj>   

    static member New(router: Router<'T>, handle: RouteHandler<'T>) =
        Sitelet.New (Router.Box router) (fun ctx ep -> 
            handle.Invoke(ctx, unbox<'T> ep)
        )

    static member Content (location: string, endpoint: 'T, cnt: Func<Context<'T>, obj>) =
        Sitelet.Content location endpoint cnt.Invoke
        
    static member Sum ([<ParamArray>] sitelets: Sitelet<'T>[]) =
        Sitelet.Sum sitelets

    static member Folder (prefix, [<ParamArray>] sitelets: Sitelet<'T>[]) =
        Sitelet.Folder prefix sitelets

type Sitelet<'T when 'T : equality> with
    member this.Box() =
        Sitelet.Box this

    member this.Protect (verifyUser: Func<string, bool>, loginRedirect: Func<'T, 'T>) =
        this |> Sitelet.Protect {
            VerifyUser = verifyUser.Invoke 
            LoginRedirect = loginRedirect.Invoke
        }

    member this.Map (embed: Func<'T, 'U>, unembed: Func<'U, 'T>) =
        Sitelet.TryMap (embed.Invoke >> ofObjNoConstraint) (unembed.Invoke >> ofObjNoConstraint) this
        
    member this.Shift (prefix: string) =
        Sitelet.Shift prefix this

[<Extension; Sealed>]
type SiteletExtensions =
    [<Extension>]
    static member Unbox<'T when 'T: equality>(sitelet: Sitelet<obj>) =
        Sitelet.Unbox<'T> sitelet

    [<Extension>]
    static member MapContent (sitelet: Sitelet<obj>, f: Func<obj, obj>) =
        { sitelet with
            Controller = fun ctx ep ->
                (ep |> sitelet.Controller ctx
                    |> Task.FromResult
                    |> f.Invoke
                )
        }

type SiteletBuilder() =

    let sitelets = ResizeArray()

    member this.With<'T when 'T : equality>(content: Func<Context<obj>, 'T, obj>) =
        sitelets.Add <|
            Sitelet.InferPartial
                box
                tryUnbox<'T>
                (fun ctx endpoint ->
                    content.Invoke(ctx, endpoint)
                    |> box
                )
        this

    member this.With(path: string, content: Func<Context<obj>, obj>) =
        let content ctx =
            content.Invoke(ctx)
        sitelets.Add <| Sitelet.Content path (box path) content
        this

    member this.Install() =
        Sitelet.Sum sitelets
