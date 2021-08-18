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
open Sitelets.RouterInferCommon

module P = FSharp.Quotations.Patterns
module S = ServerRouting

[<AutoOpen>]
module InferRouter =

    module Router =
        /// Creates a router based on type shape and WebSharper attributes.
        let Infer<'T when 'T: equality>() = 
            S.getRouter typeof<'T>
            |> ServerInferredOperators.Unbox<'T>

        /// Creates a router based on type shape and WebSharper attributes,
        /// that catches wrong method, query and request body errors.
        let InferWithCustomErrors<'T when 'T: equality>() =
            S.getRouter typeof<'T>
            |> ServerInferredOperators.IWithCustomErrors typeof<ParseRequestResult<'T>>
            |> ServerInferredOperators.Unbox<ParseRequestResult<'T>>

        /// Optimized version of Infer to use straight in a Sitelet
        let internal IInfer<'T when 'T: equality>() = 
            S.getRouter typeof<'T>
            |> ServerInferredOperators.IUnbox<'T>

        /// Optimized version of InferWithCustomErrors to use straight in a Sitelet
        let internal IInferWithCustomErrors<'T when 'T: equality>() =
            S.getRouter typeof<'T> 
            |> ServerInferredOperators.IWithCustomErrors typeof<ParseRequestResult<'T>>
            |> ServerInferredOperators.IUnbox<ParseRequestResult<'T>>