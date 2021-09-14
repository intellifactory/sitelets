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

module OpenApiIntegration =

    open System
    open System.Collections.Generic
    open System.IO
    open System.Reflection
    open ServerRouting
    open RouterInferCommon
    open OpenApiParameter
    open OpenApiPathItem
    open OpenApiPaths
    open OpenApiSchema
    open OpenApiDocument
    open OpenApiOperation
    open System.Text.Json

    type GenerateOpenApiConfig = {
            Version: string
            Title: string
            ServerUrls: string []
            SerializerOptions: JsonSerializerOptions
        }

    let generateOpenApiModel serializerOptions (endpointsType : Type) =

        //let recurringOn = HashSet()
        //let routerCache = System.Collections.Concurrent.ConcurrentDictionary<Type, OpenApiPaths>()
        let parsedClassEndpoints = Dictionary<Type, Annotation>()
        let flags = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance
        let schemaGenerator = createDefaultSchemaGeneratorFromSerializerOptions serializerOptions
        let schemaRepo = Swashbuckle.AspNetCore.SwaggerGen.SchemaRepository()

        let generateSchema (ty : Type) = 
            let schema = schemaGenerator.GenerateSchema(ty, schemaRepo)
            let ret = OpenApiSchema()
            OpenApiSchema.copyJsonSchemaProperties schema ret
            ret :> Microsoft.OpenApi.Models.OpenApiSchema

        let rec getRouter t (name: string option): OpenApiPaths option =
            //if recurringOn.Add t then
            //    let res = routerCache.GetOrAdd(t, valueFactory = fun t -> createRouter t name)
            //    recurringOn.Remove t |> ignore
            //    res
            //else
            //    //IDelayed (fun () -> routerCache.[t])
            //    routerCache.[t]
            createRouter t name
        //and wildCardRouter (t: Type) =
        //    if t = typeof<string> then
        //        //iWildcardString
        //        "*"
        //    elif t.IsArray then
        //        let item = t.GetElementType()
        //        //iWildcardArray item (getRouter item)
        //        sprintf "%A" item
        //    elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<list<_>> then
        //        let item = t.GetGenericArguments().[0]
        //        //iWildcardList item (getRouter item)
        //        sprintf "%A" item
        //    else failwithf "Invalid type for Wildcard field: %O" t

        //and getDateTimeRouter name fmt (t: Type) =
        //    if t.FullName = "System.DateTime" then
        //        //iDateTime (Some fmt)
        //        sprintf "name: %A; fmt: %A" name fmt
        //    else failwithf "Expecting a DateTime field: %s, type %s" name t.FullName

        and queryRouter (t: Type) (r: unit -> OpenApiPaths option) (name: string option) =
            let gd = if t.IsGenericType then t.GetGenericTypeDefinition() else null
            //if gd = typedefof<option<_>> then
            //    let item = t.GetGenericArguments().[0]
            //    getRouter item |> string // IQueryOption item name
            //elif gd = typedefof<Nullable<_>> then
            //    let item = t.GetGenericArguments().[0]
            //    getRouter item |> string // IQueryNullable name
            if gd = typedefof<option<_>> || gd = typedefof<Nullable<_>> then
                let item = t.GetGenericArguments().[0]
                getRouter item name // |> IQueryOption item name
            else
                // r() |> IQuery name
                let withInQuery this = withIn this (Nullable <| Microsoft.OpenApi.Models.ParameterLocation.Query)
                // TODO maybe original.Name (get rid of withNameName)
                let withNameName this = withName this name.Value
                r()
                |> Option.bind (fun original -> OpenApiPaths.mapParameters original (withInQuery >> withNameName) |> Some)


        and enumRouter (t: Type) =
            getRouter (System.Enum.GetUnderlyingType(t))

        and arrayRouter (t: Type) (_: string option) =
            let item = t.GetElementType()
            //IArray item (getRouter item)
            //sprintf "%A" item
            let paramsDict = 
                getRouter item None
                |> Option.bind (fun paths -> paths |> Seq.map (fun x -> x.Key, x.Value) |> dict |> Some)
            paramsDict
            |> Option.bind (createFromPathApiPathItemMapping >> Some)
            // or just 
            //getRouter item

        and tupleRouter (t: Type) (_: string option) =
            let items = Reflection.FSharpType.GetTupleElements t
            //let itemReader = Reflection.FSharpValue.PreComputeTupleReader t
            //let ctor = Reflection.FSharpValue.PreComputeTupleConstructor t
            //ITuple itemReader ctor
            //  (items |> Array.map getRouter)
            //|> Seq.iter (fun x -> ret.Add(x.Key, x.Value))
            let ret = OpenApiPaths()
            let collectElement t : IDictionary<string, Microsoft.OpenApi.Models.OpenApiPathItem> = 
                match getRouter t (t.Name |> Some) with
                | Some paths -> paths :> IDictionary<string, Microsoft.OpenApi.Models.OpenApiPathItem>
                | None -> dict []
            items
            |> Seq.collect collectElement
            |> Seq.iter (fun x -> ret.Add(x.Key, x.Value))
            ret
            |> Some

        and fieldRouter (t: Type) (annot: Annotation) (name: string option) : OpenApiPaths option =
            let name =
                match annot.EndPoints with
                | { Path = n } :: _ -> n
                | _ -> name |> Option.defaultValue (failwith "")
            let dateTimeFormatHandling() =
                //match annot.DateTimeFormat with
                //| Some (Choice1Of2 fmt) ->
                //    getDateTimeRouter name fmt t
                //| Some (Choice2Of2 m) when m.ContainsKey name ->
                //    getDateTimeRouter name m.[name] t
                //| _ ->
                    getRouter t (name |> Some)
            annot.Query
            |> Option.map (fun _ -> queryRouter t dateTimeFormatHandling (name |> Some))
            |> Option.defaultWith (fun () ->
                annot.FormData
                |> Option.map (fun _ -> queryRouter t dateTimeFormatHandling (name |> Some)) // |> IFormData)
                |> Option.defaultWith (fun () ->
                    annot.Json
                    |> Option.bind (fun _ -> None)
                    |> Option.defaultWith dateTimeFormatHandling
                )
            )

        and recordRouter (t: Type) (_: string option) =
            if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Cors<_>> then
                let item = t.GetGenericArguments().[0]
                //ICors item (getRouter item)
                getRouter item (item.Name |> Some)
            else
                let annot = getTypeAnnot t
                let parameters =
                    Reflection.FSharpType.GetRecordFields t
                    |> Array.choose (fun f -> fieldRouter f.PropertyType (getPropertyAnnot f) (f.Name |> Some))
                //let fieldReader = Reflection.FSharpValue.PreComputeRecordReader(t, flags)
                //let ctor = Reflection.FSharpValue.PreComputeRecordConstructor(t, flags)
                //IRecord fieldReader ctor fields
                //sprintf "fieldReader: %A; ctor: %A; fields: %A" fieldReader ctor fields
                createFromAnnotationAndPathItems annot parameters
                |> Some

        and unionCaseRouter (c: Reflection.UnionCaseInfo) (_: string option) =
            let cAnnot = getUnionCaseAnnot c
            let mutable queryFields = defaultArg cAnnot.Query Set.empty
            let mutable jsonField = cAnnot.Json |> Option.bind id
            let mutable formDataFields = defaultArg cAnnot.FormData Set.empty
            let f =
                let fields = c.GetFields()
                let indexDictionary = System.Collections.Generic.Dictionary<string, int>()
                fields |> Array.mapi (fun i f -> 
                    let fTyp = f.PropertyType
                    let fName = 
                        if fTyp.IsGenericType && fTyp.GetGenericTypeDefinition() = typedefof<Option<_>> then
                            fTyp.GetGenericArguments().[0].Name
                        else
                            f.Name
                    let fTypName = fTyp.Name
                    let nameNext =
                        let isInt (s: string) = 
                            match Int32.TryParse s with
                            | true, _ -> true
                            | _ -> false
                        if fName.StartsWith("Item") then
                            if isInt (fName.Substring(4)) then
                                indexDictionary.TryAdd(fTypName, 0) |> ignore
                                indexDictionary.[fTypName] <- indexDictionary.[fTypName] + 1
                                fTypName + " " + (indexDictionary.[fTypName] |> string)
                            else
                                fTypName
                        else
                            fName
                        |> Some
                    let r() = 
                        //match cAnnot.DateTimeFormat with
                        //| Some (Choice1Of2 fmt) ->
                        //    getDateTimeRouter fName fmt fTyp  
                        //| Some (Choice2Of2 m) when m.ContainsKey fName ->
                        //    getDateTimeRouter fName m.[fName] fTyp    
                        //| _ ->
                            getRouter fTyp nameNext
                    if queryFields.Contains fName then 
                        queryFields <- queryFields |> Set.remove fName
                        queryRouter fTyp r nameNext
                    elif formDataFields.Contains fName then 
                        formDataFields <- formDataFields |> Set.remove fName
                        queryRouter fTyp r nameNext // IFormData
                    elif Option.isSome jsonField && jsonField.Value = fName then
                        jsonField <- None
                        //getJsonRouter fTyp
                        // TODO
                        None
                    elif fTyp.IsGenericType && fTyp.GetGenericTypeDefinition() = typedefof<Option<_>> then
                        systemRouter fTyp nameNext
                    //elif cAnnot.IsWildcard && i = fields.Length - 1 then
                    //    wildCardRouter fTyp
                    else r()
                )
            if queryFields.Count > 0 then
                failwithf "Query field not found: %s" (Seq.head queryFields)
            match jsonField with
            | Some j ->
                failwithf "Json field not found: %s" j
            | _ -> ()
            if formDataFields.Count > 0 then
                failwithf "FormData field not found: %s" (Seq.head formDataFields)
            // todo: more error reports
            //sprintf "endpoints: %A; fields: %A; hasWildCard: %A" endpoints f cAnnot.IsWildcard
            //{ EndPoints = endpoints; Fields = f; HasWildCard = cAnnot.IsWildcard }
            f
            |> Array.choose id
            |> createFromAnnotationAndPathItems cAnnot 
            |> Some

        and unionRouter (t: Type) (_: string option) =
            //if isGen && t.GetGenericTypeDefinition() = typedefof<list<_>> then
            //    let item = t.GetGenericArguments().[0]
            //    IList item (getRouter item)
            //elif isGen && t.GetGenericTypeDefinition() = typedefof<ParseRequestResult<_>> then
            //    let item = t.GetGenericArguments().[0]
            //    IWithCustomErrors t (getRouter item)
            let isGen = t.IsGenericType
            if isGen && (t.GetGenericTypeDefinition() = typedefof<list<_>> || t.GetGenericTypeDefinition() = typedefof<ParseRequestResult<_>>) then
                let item = t.GetGenericArguments().[0]
                getRouter item None
            else
                let cases = Reflection.FSharpType.GetUnionCases(t, flags)
                //let tagReader = Reflection.FSharpValue.PreComputeUnionTagReader(t, flags)
                //let caseReaders = 
                //    cases |> Array.map (fun c -> Reflection.FSharpValue.PreComputeUnionReader(c, flags))
                //let caseCtors = 
                //    cases |> Array.map (fun c -> Reflection.FSharpValue.PreComputeUnionConstructor(c, flags))
                //IUnion caseReaders caseCtors (cases |> Array.map unionCaseRouter)
                let parameters =
                    cases
                    |> Array.choose (fun x ->
                        let ret = unionCaseRouter x None
                        ret)
                    |> Seq.collect (fun x -> Seq.zip x.Keys (x.Values |> Seq.cast))
                    |> dict
                createFromPathPathItemMapping parameters
                |> Some


        and systemRouter (t: Type) (name: string option) =

            let openApiOperation =
                createDefaultFromSchemaAndName (generateSchema t) (name |> Option.defaultValue t.Name)

            let pathItem =
                withOperations (OpenApiPathItem()) <| dict [
                        (Microsoft.OpenApi.Models.OperationType.Get, openApiOperation)
                    ]
            createFromPathPathItemMapping <| dict [
                (t.Name, pathItem)
                ]
            |> Some
            //match t.Name with
            //| "Object" -> IEmpty
            //| "String" -> iString
            //| "Char" -> iChar
            //| "Guid" -> iGuid
            //| "Boolean" -> iBool
            //| "Int32" -> iInt
            //| "Double" -> iDouble
            //| "DateTime" -> iDateTime None
            //| "SByte" -> iSByte
            //| "Byte" -> iByte
            //| "Int16" -> iInt16
            //| "UInt16" -> iUInt16
            //| "UInt32" -> iUInt
            //| "Int64" -> iInt64
            //| "UInt64" -> iUInt64
            //| "Single" -> iSingle
            //| "Nullable`1" ->
            //    let item = t.GetGenericArguments().[0]
            //    sprintf "nullable %A" (getRouter item)
            //| n ->
            //    failwithf "System type not supported for inferred router: %s" n

        and classRouter (t: Type) _ =
            let rec getClassAnnotation td : Annotation =
                match parsedClassEndpoints.TryGetValue(td) with
                | true, ep -> ep
                | false, _ ->
                    let b =
                        let b = t.BaseType
                        if b.FullName = "System.Object" then None else Some (getClassAnnotation b)
                    let thisAnnot = getTypeAnnot t
                    let annot =
                        match b with
                        | Some b -> Annotation.Combine b thisAnnot
                        | _ -> thisAnnot
                    parsedClassEndpoints.Add(td, annot)
                    annot
            let annot = getClassAnnotation t
            let endpoints =
                annot.EndPoints |> List.map (fun e ->
                    e.Method, Route.FromUrl e.Path |> GetPathHoles |> fst
                )
            let endpoints =
                if List.isEmpty endpoints then [None, [||]] else endpoints
            let allFieldsArr =
                t.GetFields(BindingFlags.Instance ||| BindingFlags.Public)
                |> Array.map (fun f ->
                    f.Name,
                    (f, getFieldAnnot f)
                )
            let allFields = dict allFieldsArr
            let allQueryFields =
                allFieldsArr |> Seq.choose (
                    function
                    | fn, (_, { Query = Some _ }) -> Some fn
                    | _ -> None
                ) |> Set
            let routedFieldNames =
                endpoints |> Seq.collect (fun (_, ep) ->
                    ep |> Seq.choose (fun s ->
                        match s with
                        | StringSegment _ -> None
                        | FieldSegment fName -> Some fName
                    )
                ) |> Seq.append allQueryFields |> Seq.distinct |> Array.ofSeq
            let fieldIndexes =
                routedFieldNames |> Seq.mapi (fun i n -> n, i) |> dict
            let fieldRouters =
                routedFieldNames
                |> Seq.map (fun fName ->
                    let field, fAnnot = allFields.[fName]
                    fieldRouter field.FieldType fAnnot (fName |> Some)
                )
                |> Array.ofSeq
            let fields =
                routedFieldNames
                |> Seq.map (fun fName ->
                    fst allFields.[fName]
                )
                |> Array.ofSeq
            let partsAndFields =
                endpoints |> Seq.map (fun (m, ep) ->
                    let mutable queryFields = allQueryFields
                    let explicitSegments =
                        ep |> Seq.map (function
                        | StringSegment s -> Choice1Of2 s
                        | FieldSegment fName ->
                            queryFields <- queryFields.Remove fName
                            Choice2Of2 fieldIndexes.[fName]
                        ) |> Array.ofSeq
                    m,
                    Array.append explicitSegments (
                        queryFields |> Seq.map (fun f -> Choice2Of2 fieldIndexes.[f]) |> Array.ofSeq
                    )
                ) |> Array.ofSeq
            let readFields (o: obj) =
                fields |> Array.map (fun f -> f.GetValue(o))
            let createObject values =
                let o = System.Activator.CreateInstance(t)
                (fields, values) ||> Array.iter2 (fun f v -> f.SetValue(o, v))
                o
            let subClasses =
                t.GetNestedTypes() |> Array.choose (fun nt ->
                    if nt.BaseType = t then Some (nt, getRouter nt) else None
                )
            //IClass readFields createObject fieldRouters partsAndFields subClasses
            // TODO
            None

        and createRouter (t: Type) =
            if t.IsEnum then enumRouter t
            elif t.IsArray then arrayRouter t
            elif Reflection.FSharpType.IsTuple t then tupleRouter t
            elif Reflection.FSharpType.IsRecord t then recordRouter t
            elif Reflection.FSharpType.IsUnion t then unionRouter t
            elif t.Namespace = "System" then systemRouter t
            else classRouter t
        
        if Reflection.FSharpType.IsUnion endpointsType then
            Ok (schemaRepo, unionRouter endpointsType None)
        else
            Error (schemaRepo, getRouter endpointsType None, "EndPoint's type must be Discriminated Union")

    let generateOpenApiDefault serializerOptions (endpointsType : Type) =
        match generateOpenApiModel serializerOptions endpointsType with
        | Ok (schemaRepo, pathsOption) -> 
            match pathsOption with
            | Some paths ->
                let document = OpenApiDocument.createDefaultFromPaths paths
                document.Components <- Microsoft.OpenApi.Models.OpenApiComponents(Schemas = schemaRepo.Schemas)
                Ok document
            | None -> 
                Error (schemaRepo, pathsOption, "None of the endpoints' type were implemented")
        | Error x ->
            Error x

    let generateOpenApi config endpointsType =
        match generateOpenApiModel config.SerializerOptions endpointsType with
        | Ok (schemaRepo, pathsOption) -> 
            match pathsOption with
            | Some paths ->
                let document = OpenApiDocument.createFromRecord {
                    Version = config.Version
                    Title = config.Title
                    ServerUrls = config.ServerUrls
                    Paths = paths
                    SchemaRepo = schemaRepo
                    }
                document.Components <- Microsoft.OpenApi.Models.OpenApiComponents(Schemas = schemaRepo.Schemas)
                Ok document
            | None -> 
                Error (schemaRepo, pathsOption, "None of the endpoints' type were implemented")
        | Error x ->
            Error x

    let generateSwaggerJsonBytes config (endpointsType : Type) =
        match generateOpenApi config endpointsType with
        | Ok document -> toBytes document
        | Error (_, _, error) -> failwith error

