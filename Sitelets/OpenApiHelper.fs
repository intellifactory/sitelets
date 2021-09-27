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

open System.IO
open Microsoft.OpenApi.Writers
open System.Collections.Generic
open System
open System.Globalization
open Microsoft.OpenApi.Extensions

module OpenApiSchema =
    open System.Reflection

    type OpenApiSchema() =
        inherit Microsoft.OpenApi.Models.OpenApiSchema()

        interface System.IEquatable<OpenApiSchema> with
            member this.Equals (other: OpenApiSchema) =
                this.AdditionalProperties = other.AdditionalProperties &&
                this.AdditionalPropertiesAllowed = other.AdditionalPropertiesAllowed &&
                //this.AllOf = other.AllOf &&
                //this.AnyOf = other.AnyOf &&
                this.Default = other.Default &&
                this.Deprecated = other.Deprecated &&
                this.Description = other.Description &&
                this.Discriminator = other.Discriminator &&
                this.Enum.Count = other.Enum.Count &&
                Seq.forall2 (=) this.Enum other.Enum &&
                this.Example = other.Example &&
                this.ExclusiveMaximum = other.ExclusiveMaximum &&
                this.ExclusiveMinimum = other.ExclusiveMinimum &&
                this.Extensions.Count = other.Extensions.Count &&
                Seq.forall2 (=) this.Extensions other.Extensions &&
                this.ExternalDocs = other.ExternalDocs &&
                this.Format = other.Format &&
                this.Items = other.Items &&
                this.MaxItems = other.MaxItems &&
                this.MaxLength = other.MaxLength &&
                this.MaxProperties = other.MaxProperties &&
                this.Maximum = other.Maximum &&
                this.MinItems = other.MinItems &&
                this.MinLength = other.MinLength &&
                this.MinProperties = other.MinProperties &&
                this.Minimum = other.Minimum &&
                this.MultipleOf = other.MultipleOf &&
                this.Not = other.Not &&
                this.Nullable = other.Nullable &&
                //this.OneOf = other.OneOf &&
                this.Pattern = other.Pattern &&
                //this.Properties = other.Properties &&
                this.ReadOnly = other.ReadOnly &&
                this.Reference = other.Reference &&
                this.Required.Count = other.Required.Count &&
                Seq.forall2 (=) this.Required other.Required &&
                this.Title = other.Title &&
                this.Type = other.Type &&
                this.UniqueItems = other.UniqueItems &&
                this.UnresolvedReference = other.UnresolvedReference &&
                this.WriteOnly = other.WriteOnly &&
                this.Xml = other.Xml

        static member op_Equality ((this: IEquatable<OpenApiSchema>), (other: OpenApiSchema)) =
            this.Equals(other)

        override self.ToString() =
            use outputString = new StringWriter(CultureInfo.InvariantCulture)
            let writer = new OpenApiJsonWriter(outputString)
            self.SerializeAsV3WithoutReference(writer)
            outputString.GetStringBuilder().ToString()
    
    type NoReadOnlyPropertiesFilter() =
        interface Swashbuckle.AspNetCore.SwaggerGen.ISchemaFilter with
            member _.Apply(schema, _) =
                for prop in schema.Properties do
                    prop.Value.ReadOnly <- false

    let copyJsonSchemaProperties (source : Microsoft.OpenApi.Models.OpenApiSchema) (target : OpenApiSchema) =
        target.AdditionalProperties <- source.AdditionalProperties
        target.AdditionalPropertiesAllowed <- source.AdditionalPropertiesAllowed
        target.AllOf <- source.AllOf
        target.AnyOf <- source.AnyOf
        target.Default <- source.Default
        target.Deprecated <- source.Deprecated
        target.Description <- source.Description
        target.Discriminator <- source.Discriminator
        target.Enum <- source.Enum
        target.Example <- source.Example
        target.ExclusiveMaximum <- source.ExclusiveMaximum
        target.ExclusiveMinimum <- source.ExclusiveMinimum
        target.Extensions <- source.Extensions
        target.ExternalDocs <- source.ExternalDocs
        target.Format <- source.Format
        target.Items <- source.Items
        target.MaxItems <- source.MaxItems
        target.MaxLength <- source.MaxLength
        target.MaxProperties <- source.MaxProperties
        target.Maximum <- source.Maximum
        target.MinItems <- source.MinItems
        target.MinLength <- source.MinLength
        target.MinProperties <- source.MinProperties
        target.Minimum <- source.Minimum
        target.MultipleOf <- source.MultipleOf
        target.Not <- source.Not
        target.Nullable <- source.Nullable
        target.OneOf <- source.OneOf
        target.Pattern <- source.Pattern
        target.Properties <- source.Properties
        target.ReadOnly <- source.ReadOnly
        target.Reference <- source.Reference
        target.Required <- source.Required
        target.Title <- source.Title
        target.Type <- source.Type
        target.UniqueItems <- source.UniqueItems
        target.UnresolvedReference <- source.UnresolvedReference
        target.WriteOnly <- source.WriteOnly
        target.Xml <- source.Xml

    let private isOptionType (ty : Type) =
        ty.GetTypeInfo().IsGenericType && ty.GetGenericTypeDefinition() = typedefof<option<_>>

    type OptionsAsNullableValuesFilter() =
        interface Swashbuckle.AspNetCore.SwaggerGen.ISchemaFilter with
            member _.Apply(schema, context) =
                if isOptionType context.Type then
                    let schema = schema :?> OpenApiSchema
                    let valueSchema = Seq.head schema.Properties.Values :?> OpenApiSchema
                    copyJsonSchemaProperties valueSchema schema
                    schema.Nullable <- true


    let private getActualSchema (schemaRepository : Swashbuckle.AspNetCore.SwaggerGen.SchemaRepository) (schema : Microsoft.OpenApi.Models.OpenApiSchema) =
        if isNull schema.Reference then
            schema
        else
            schemaRepository.Schemas.[schema.Reference.Id]


    type RequiredIfNotNullableFilter() =
        interface Swashbuckle.AspNetCore.SwaggerGen.ISchemaFilter with
            member _.Apply(schema, context) =
                for KeyValue(propName, propSchema) in schema.Properties do
                    let actualPropSchema = getActualSchema context.SchemaRepository propSchema
                    if not actualPropSchema.Nullable then
                        schema.Required.Add(propName) |> ignore



    let createDefaultSchemaGeneratorFromSerializerOptions serializerOptions =
        let schemaGeneratorOptions = Swashbuckle.AspNetCore.SwaggerGen.SchemaGeneratorOptions()
        schemaGeneratorOptions.SchemaFilters.Add(NoReadOnlyPropertiesFilter())
        schemaGeneratorOptions.SchemaFilters.Add(OptionsAsNullableValuesFilter())
        schemaGeneratorOptions.SchemaFilters.Add(RequiredIfNotNullableFilter())

        let dataContractResolver = Swashbuckle.AspNetCore.SwaggerGen.JsonSerializerDataContractResolver(serializerOptions)
        Swashbuckle.AspNetCore.SwaggerGen.SchemaGenerator(schemaGeneratorOptions, dataContractResolver)

    type OpenApiSchema with
        static member CreateDefaultSchemaGeneratorFromSerializerOptions = createDefaultSchemaGeneratorFromSerializerOptions

module OpenApiParameter =

    type T = { Required: bool
               In: Nullable<Microsoft.OpenApi.Models.ParameterLocation>
               Schema: Microsoft.OpenApi.Models.OpenApiSchema
               Name: string }

    type OpenApiParameter() =
        inherit Microsoft.OpenApi.Models.OpenApiParameter()
        interface System.IEquatable<OpenApiParameter> with
            member this.Equals other = 
                this.UnresolvedReference = other.UnresolvedReference &&
                this.Reference = other.Reference &&
                this.Name = other.Name &&
                this.In = other.In &&
                this.Description = other.Description &&
                this.Required = other.Required &&
                this.Deprecated = other.Deprecated &&
                this.AllowEmptyValue = other.AllowEmptyValue &&
                this.Style = other.Style &&
                this.Explode = other.Explode &&
                this.AllowReserved = other.AllowReserved &&
                OpenApiSchema.OpenApiSchema.(=)((this.Schema :?> OpenApiSchema.OpenApiSchema), (other.Schema :?> OpenApiSchema.OpenApiSchema)) &&
                this.Examples.Count = other.Examples.Count &&
                Seq.forall2 (=) this.Examples other.Examples &&
                this.Example = other.Example &&
                this.Content.Count = other.Content.Count &&
                Seq.forall2 (=) this.Content other.Content &&
                this.Extensions.Count = other.Extensions.Count &&
                Seq.forall2 (=) this.Extensions other.Extensions

        static member op_Equality ((this: IEquatable<OpenApiParameter>), (other: OpenApiParameter)) =
            this.Equals(other)

        override self.ToString() =
            use outputString = new StringWriter(CultureInfo.InvariantCulture)
            let writer = new OpenApiJsonWriter(outputString)
            self.SerializeAsV3WithoutReference(writer)
            outputString.GetStringBuilder().ToString()

    let createFromRecord (record: T) =
        OpenApiParameter(
            Required = record.Required,
            In = record.In,
            Schema = record.Schema,
            Name = record.Name)

    let createDefaultFromSchemaAndName schema name = 
        createFromRecord
            { 
                Required = true
                In = Nullable(Microsoft.OpenApi.Models.ParameterLocation.Path)
                Schema = schema
                Name = name
            }


    let withIn (this: OpenApiParameter) (paramIn: Nullable<Microsoft.OpenApi.Models.ParameterLocation>) =
        OpenApiParameter(
            UnresolvedReference = this.UnresolvedReference,
            Reference = this.Reference,
            Name = this.Name,
            In = paramIn,
            Description = this.Description,
            Required = this.Required,
            Deprecated = this.Deprecated,
            AllowEmptyValue = this.AllowEmptyValue,
            Style = this.Style,
            Explode = this.Explode,
            AllowReserved = this.AllowReserved,
            Schema = this.Schema,
            Examples = this.Examples,
            Example = this.Example,
            Content = this.Content,
            Extensions = this.Extensions
            )

    let withName (this: OpenApiParameter) (name: string) =
        OpenApiParameter(
            UnresolvedReference = this.UnresolvedReference,
            Reference = this.Reference,
            Name = name,
            In = this.In,
            Description = this.Description,
            Required = this.Required,
            Deprecated = this.Deprecated,
            AllowEmptyValue = this.AllowEmptyValue,
            Style = this.Style,
            Explode = this.Explode,
            AllowReserved = this.AllowReserved,
            Schema = this.Schema,
            Examples = this.Examples,
            Example = this.Example,
            Content = this.Content,
            Extensions = this.Extensions
            )

    type OpenApiParameter with
        static member CreateFromRecord = createFromRecord 
        static member CreateDefaultFromSchemaAndName = createDefaultFromSchemaAndName 
        member this.WithIn = withIn this
        member this.WithName = withName this

module OpenApiResponses =
    type OpenApiResponses() =
        inherit Microsoft.OpenApi.Models.OpenApiResponses()

        override self.ToString() =
            self.SerializeAsJson(Microsoft.OpenApi.OpenApiSpecVersion.OpenApi3_0)

    ///<summary>Create a Responses object from "status code", Microsoft.OpenApi.Models.OpenApiResponse dictionary</summary>
    ///<param name="responses">dictionary where the keys are status codes, the values are OpenApiResponses</param>
    ///<returns>OpenApiResponses, where the keys are status codes, the values are OpenApiResponses</returns>
    let createDefaultFromResponses (responses: IDictionary<string, Microsoft.OpenApi.Models.OpenApiResponse>) =
        let openApiResponses = OpenApiResponses()
        for KeyValue(path, openApiPathItem) in responses do
            openApiResponses.Add(path, openApiPathItem)
        openApiResponses

    let createDefault =
        //openApiResponses.Add("200", 
        //    OpenApiResponse(
        //        Content = dict [
        //            for requestBodyMetadata in responseBodies do
        //                let schema = generateSchema requestBodyMetadata.ResponseType
        //                (requestBodyMetadata.MimeType, OpenApiMediaType(Schema = schema))
        //        ]))

        let openApiMediaType = Microsoft.OpenApi.Models.OpenApiMediaType()
        let openApiResponse = Microsoft.OpenApi.Models.OpenApiResponse()
        openApiResponse.Content.Add("text/html", openApiMediaType)
        openApiResponse.Description <- String.Empty

        createDefaultFromResponses <| dict [ ("200", openApiResponse) ]

    type OpenApiResponses with
        static member CreateDefaultFromResponses = createDefaultFromResponses  
        static member CreateDefault = createDefault


module OpenApiOperation =

    type T = { Summary: string
               Description: string
               Parameters: IList<OpenApiParameter.OpenApiParameter>
               RequestBody: Microsoft.OpenApi.Models.OpenApiRequestBody
               Responses: Microsoft.OpenApi.Models.OpenApiResponses }
    
    type OpenApiOperation() =
        inherit Microsoft.OpenApi.Models.OpenApiOperation()
        interface System.IEquatable<OpenApiOperation> with
            member this.Equals other = 
                //this.Tags.Count = other.Tags.Count &&
                //Seq.forall2 (=) this.Tags other.Tags &&
                this.Summary = other.Summary &&
                this.Description = other.Description &&
                //this.ExternalDocs = other.ExternalDocs &&
                this.OperationId = other.OperationId &&
                this.Parameters.Count = other.Parameters.Count &&
                Seq.forall2 (fun (x: Microsoft.OpenApi.Models.OpenApiParameter) (y: Microsoft.OpenApi.Models.OpenApiParameter) ->
                    let thisParam = x :?> OpenApiParameter.OpenApiParameter
                    let otherParam = y :?> OpenApiParameter.OpenApiParameter
                    OpenApiParameter.OpenApiParameter.(=)(thisParam, otherParam)) this.Parameters other.Parameters &&
                this.RequestBody = other.RequestBody &&
                (this.Responses :?> OpenApiResponses.OpenApiResponses) = (other.Responses :?> OpenApiResponses.OpenApiResponses) &&
                this.Callbacks.Count = other.Callbacks.Count &&
                Seq.forall2 (=) this.Callbacks other.Callbacks &&
                this.Deprecated = other.Deprecated
                //this.Security.Count = other.Security.Count &&
                //Seq.forall2 (=) this.Security other.Security &&
                //this.Servers.Count = other.Servers.Count &&
                //Seq.forall2 (=) this.Servers other.Servers &&
                //this.Extensions.Count = other.Extensions.Count &&
                //Seq.forall2 (=) this.Extensions other.Extensions 

        static member op_Equality ((this: IEquatable<OpenApiOperation>), (other: OpenApiOperation)) =
            this.Equals(other)

        override self.ToString() =
            self.SerializeAsJson(Microsoft.OpenApi.OpenApiSpecVersion.OpenApi3_0)

    let convertParameters (parameters: IList<OpenApiParameter.OpenApiParameter>): ResizeArray<Microsoft.OpenApi.Models.OpenApiParameter> =
        parameters |> Seq.map (fun x -> x :> Microsoft.OpenApi.Models.OpenApiParameter) |> ResizeArray

    let createFromRecord (record: T) = 
        OpenApiOperation(
            Summary = record.Summary,
            Description = record.Description,
            Parameters = convertParameters record.Parameters,
            RequestBody = record.RequestBody,
            Responses = record.Responses
            )

    let createDefaultFromSchemaAndName schema name =
        let openApiParameter =
            OpenApiParameter.createDefaultFromSchemaAndName schema name
        createFromRecord {
                Summary = Option.toObj None
                Description = Option.toObj None
                Parameters = ResizeArray([ openApiParameter ])
                RequestBody = Option.toObj None
                Responses = OpenApiResponses.createDefault
            }

    let withParameters (this: OpenApiOperation) (parameters: IList<OpenApiParameter.OpenApiParameter>) =
        OpenApiOperation(
             Tags = this.Tags,
             Summary = this.Summary,
             Description = this.Description,
             ExternalDocs = this.ExternalDocs,
             OperationId = this.OperationId,
             Parameters = convertParameters parameters,
             RequestBody = this.RequestBody,
             Responses = this.Responses,
             Callbacks = this.Callbacks,
             Deprecated = this.Deprecated,
             Security = this.Security,
             Servers = this.Servers,
             Extensions = this.Extensions 
            )

    let withResponses (this: OpenApiOperation) (responses: OpenApiResponses.OpenApiResponses) =
        OpenApiOperation(
             Tags = this.Tags,
             Summary = this.Summary,
             Description = this.Description,
             ExternalDocs = this.ExternalDocs,
             OperationId = this.OperationId,
             Parameters = this.Parameters,
             RequestBody = this.RequestBody,
             Responses = (responses :> Microsoft.OpenApi.Models.OpenApiResponses),
             Callbacks = this.Callbacks,
             Deprecated = this.Deprecated,
             Security = this.Security,
             Servers = this.Servers,
             Extensions = this.Extensions 
            )

    let mapParameters (this: OpenApiOperation) (mapping: OpenApiParameter.OpenApiParameter -> OpenApiParameter.OpenApiParameter) =
        let createParam (param: Microsoft.OpenApi.Models.OpenApiParameter) =
            OpenApiParameter.createFromRecord { Required = param.Required; In = param.In; Schema = param.Schema; Name = param.Name }
        let parameters: ResizeArray<OpenApiParameter.OpenApiParameter> =
            (Seq.map (createParam >> mapping) this.Parameters)
            |> Seq.cast
            |> ResizeArray
        withParameters this parameters

    type OpenApiOperation with
        static member CreateFromRecord = createFromRecord 
        static member CreateDefaultFromSchemaAndName = createDefaultFromSchemaAndName 
        member this.WithParameters = withParameters this
        member this.WithResponses = withResponses this
        member this.MapParameters = mapParameters this


module OpenApiPathItem =

    type T = { Description: string
               Extensions: IDictionary<string, Microsoft.OpenApi.Interfaces.IOpenApiExtension>
               Parameters: IList<Microsoft.OpenApi.Models.OpenApiParameter>
               Servers: IList<Microsoft.OpenApi.Models.OpenApiServer>
               Summary: string
               Operations: IDictionary<Microsoft.OpenApi.Models.OperationType, Microsoft.OpenApi.Models.OpenApiOperation>
              }

    type OpenApiPathItem() =
        inherit Microsoft.OpenApi.Models.OpenApiPathItem()
        interface System.IEquatable<OpenApiPathItem> with
            member this.Equals other = 
                this.Description = other.Description &&
                //this.Extensions = other.Extensions &&
                //this.Parameters = other.Parameters &&
                //this.Servers = other.Servers &&
                this.Summary = other.Summary &&
                this.Operations.Count = other.Operations.Count &&
                Seq.forall2 (fun (x: KeyValuePair<Microsoft.OpenApi.Models.OperationType, Microsoft.OpenApi.Models.OpenApiOperation>) (y: KeyValuePair<Microsoft.OpenApi.Models.OperationType, Microsoft.OpenApi.Models.OpenApiOperation>) -> 
                                x.Key = y.Key && OpenApiOperation.OpenApiOperation.(=)((x.Value :?> OpenApiOperation.OpenApiOperation), (y.Value :?> OpenApiOperation.OpenApiOperation))) this.Operations other.Operations

        static member op_Equality ((this: IEquatable<OpenApiPathItem>), (other: OpenApiPathItem)) =
            this.Equals(other)

        override self.ToString() =
            self.SerializeAsJson(Microsoft.OpenApi.OpenApiSpecVersion.OpenApi3_0)

    let createFromRecord (record: T) =
        OpenApiPathItem(
            Description = record.Description,
            Extensions = record.Extensions,
            Parameters = record.Parameters,
            Servers = record.Servers,
            Summary = record.Summary,
            Operations = record.Operations
            )

    let withOperations (this: OpenApiPathItem) (operations: IDictionary<Microsoft.OpenApi.Models.OperationType, OpenApiOperation.OpenApiOperation>) =
        OpenApiPathItem(
            Description = this.Description,
            Extensions = this.Extensions,
            Parameters = this.Parameters,
            Servers = this.Servers,
            Summary = this.Summary,
            Operations = dict [
                for KeyValue(k, v) in operations do
                    k, v :> Microsoft.OpenApi.Models.OpenApiOperation
                ]
            )

    let mapParameters (this: OpenApiPathItem) (mapping: OpenApiParameter.OpenApiParameter -> OpenApiParameter.OpenApiParameter) =
        let operations = dict [
            for KeyValue(k, v) in this.Operations do
                let newParameter = OpenApiOperation.mapParameters (v :?> OpenApiOperation.OpenApiOperation) mapping
                k, newParameter
            ]
        withOperations this operations


    type OpenApiPathItem with
        static member CreateFromRecord = createFromRecord 
        member this.WithOperations = withOperations this
        member this.MapParameters = mapParameters this

module OpenApiPaths =

    type OpenApiPaths() =
        inherit Microsoft.OpenApi.Models.OpenApiPaths()
        interface System.IEquatable<OpenApiPaths> with
            member this.Equals other = 
                this.Count = other.Count &&
                Seq.forall2 (fun (x: KeyValuePair<string, Microsoft.OpenApi.Models.OpenApiPathItem>) (y: KeyValuePair<string, Microsoft.OpenApi.Models.OpenApiPathItem>) -> 
                                x.Key = y.Key && OpenApiPathItem.OpenApiPathItem.(=)((x.Value :?> OpenApiPathItem.OpenApiPathItem), (y.Value :?> OpenApiPathItem.OpenApiPathItem))) this other

        static member op_Equality ((this: IEquatable<OpenApiPaths>), (other: OpenApiPaths)) =
            this.Equals(other)

        override self.ToString() =
            self.SerializeAsJson(Microsoft.OpenApi.OpenApiSpecVersion.OpenApi3_0)

    let createFromPathApiPathItemMapping (parameters: IDictionary<string, Microsoft.OpenApi.Models.OpenApiPathItem>) =
        let openApiPaths = OpenApiPaths()
        for KeyValue(path, openApiPathItem) in parameters do
            openApiPaths.Add(path, OpenApiPathItem.createFromRecord {
                    Description = openApiPathItem.Description
                    Extensions = openApiPathItem.Extensions
                    Parameters = openApiPathItem.Parameters
                    Servers = openApiPathItem.Servers
                    Summary = openApiPathItem.Summary
                    Operations = openApiPathItem.Operations
                })
        openApiPaths

    ///<summary>Create a Paths object from "path", OpenApiPathItem.OpenApiPathItem dictionary</summary>
    ///<param name="parameters">dictionary where the keys are paths, the values are OpenApiPathItems</param>
    ///<returns>OpenApiPaths, where the keys are paths, the values are OpenApiPathItems</returns>
    let createFromPathPathItemMapping (parameters: IDictionary<string, OpenApiPathItem.OpenApiPathItem>) =
        let openApiPaths = OpenApiPaths()
        for KeyValue(path, openApiPathItem) in parameters do
            openApiPaths.Add(path, OpenApiPathItem.createFromRecord {
                    Description = openApiPathItem.Description
                    Extensions = openApiPathItem.Extensions
                    Parameters = openApiPathItem.Parameters
                    Servers = openApiPathItem.Servers
                    Summary = openApiPathItem.Summary
                    Operations = openApiPathItem.Operations
                })
        openApiPaths

    ///<summary>Create a Paths object from endpoint/method attribute annotation and operations list</summary>
    ///<param name="annot">An annotation usually coming from getXYAnnot from ServerRouting. Must contain an endpoint with correct Method and Path</param>
    ///<param name="paths">OpenApiPaths, where the Operations inside are used for Values in the new OpenApiPaths</param>
    ///<returns>OpenApiPaths, where the key is method defaulted to get, the values are OpenApiPathItems with the operations given</returns>
    let internal createFromAnnotationAndPathItems (annot: Sitelets.RouterInferCommon.Annotation) (paths: OpenApiPaths array) =
        let firstEndPoint = annot.EndPoints.[0]
        let endpointType = 
            match firstEndPoint.Method with
            | Some x -> pascalCase x
            | None -> "Get"
            |> Enum.Parse<Microsoft.OpenApi.Models.OperationType>
        let endpointString = if firstEndPoint.Path.StartsWith "/" then firstEndPoint.Path else "/" + firstEndPoint.Path
        if paths.Length = 0 then
            let operation = OpenApiOperation.withResponses (OpenApiOperation.OpenApiOperation()) OpenApiResponses.createDefault
            let endPoint = OpenApiPathItem.withOperations (OpenApiPathItem.OpenApiPathItem()) <| dict [ (endpointType, operation) ]
            createFromPathPathItemMapping <| dict [endpointString, endPoint] 
        else
            let nonQueryParams (parameters: Microsoft.OpenApi.Models.OpenApiParameter []) = 
                parameters |> Array.filter (fun x ->
                    match Option.ofNullable x.In with
                    | Some Microsoft.OpenApi.Models.ParameterLocation.Path -> true
                    | _ -> false)
            paths
            |> Seq.collect id
            |> Seq.collect (fun x -> 
                if x.Value.Operations.Count = 0 then
                    Seq.singleton (String.Empty, (OpenApiOperation.withResponses (OpenApiOperation.OpenApiOperation()) OpenApiResponses.createDefault))
                else
                    x.Value.Operations
                    |> Seq.map (fun y ->
                        let operation = y.Value :?> OpenApiOperation.OpenApiOperation
                        let parameters =
                            operation.Parameters
                            |> Seq.toArray
                        let paramsString =
                            let pathParams = nonQueryParams parameters
                            if pathParams.Length = 0 then
                                if parameters.Length > 0 then
                                    // leave out the query param
                                    String.Empty
                                else
                                    x.Key
                            else
                                "/" + String.Join("/", pathParams |> Array.map (fun x -> "{" + x.Name + "}"))
                        (paramsString, operation)
                        )
                )
            |> Seq.fold (fun (path, acc) (pString, op) -> (path + pString, op::acc)) (endpointString, List.empty) 
            |> (fun (path, (acc: OpenApiOperation.OpenApiOperation list)) -> 
                let parameters = acc |> List.rev |> Seq.collect (fun x -> x.Parameters |> Seq.cast) |> ResizeArray
                let operation = OpenApiOperation.withParameters (List.head acc) parameters
                let endPoint = OpenApiPathItem.withOperations (OpenApiPathItem.OpenApiPathItem()) <| dict [ (endpointType, operation) ]
                [ (path, endPoint) ]
                )
            |> dict
            |> createFromPathPathItemMapping 

    let mapParameters (this: OpenApiPaths) (mapping: OpenApiParameter.OpenApiParameter -> OpenApiParameter.OpenApiParameter) =
        let newParameters = dict [
            for KeyValue(k, v) in this do
                let newParameter = OpenApiPathItem.mapParameters (v :?> OpenApiPathItem.OpenApiPathItem) mapping
                k, newParameter
                ]
        createFromPathPathItemMapping newParameters

    type OpenApiPaths with
        static member CreateFromPathApiPathItemMapping = createFromPathApiPathItemMapping
        static member internal CreateFromAnnotationAndPathItems = createFromAnnotationAndPathItems  
        static member CreateFromPathPathItemMapping = createFromPathPathItemMapping 
        member this.MapOperationParameters = mapParameters this

module OpenApiDocument =
    type T = {
        Version: string
        Title: string
        ServerUrls: string []
        Paths: Microsoft.OpenApi.Models.OpenApiPaths
        SchemaRepo: Swashbuckle.AspNetCore.SwaggerGen.SchemaRepository }

    type OpenApiDocument() =
        inherit Microsoft.OpenApi.Models.OpenApiDocument()
        interface System.IEquatable<OpenApiDocument> with
            member this.Equals other = 
                //this.Components = other.Components &&
                this.Extensions.Count = other.Extensions.Count &&
                Seq.forall2 (=) this.Extensions other.Extensions &&
                this.ExternalDocs = other.ExternalDocs &&
                this.Info.Title = other.Info.Title &&
                this.Info.Version = other.Info.Version &&
                this.Info.Description = other.Info.Description &&
                OpenApiPaths.OpenApiPaths.(=)((this.Paths :?> OpenApiPaths.OpenApiPaths), (other.Paths :?> OpenApiPaths.OpenApiPaths))
                //this.SecurityRequirements.Count = other.SecurityRequirements.Count &&
                //Seq.forall2 (=) this.SecurityRequirements other.SecurityRequirements &&
                ////this.Servers.Count = other.Servers.Count &&
                ////Seq.forall2 (=) this.Servers other.Servers &&
                //this.Tags.Count = other.Tags.Count &&
                //Seq.forall2 (=) this.Tags other.Tags

        static member op_Equality ((this: IEquatable<OpenApiDocument>), (other: OpenApiDocument)) =
            this.Equals(other)

        override self.ToString() =
            self.SerializeAsJson(Microsoft.OpenApi.OpenApiSpecVersion.OpenApi3_0)

    let withPaths (this: OpenApiDocument) (paths: OpenApiPaths.OpenApiPaths) =
        OpenApiDocument(
            Components = this.Components,
            Extensions = this.Extensions,
            ExternalDocs = this.ExternalDocs,
            Info = this.Info,
            Paths = paths,
            SecurityRequirements = this.SecurityRequirements,
            Servers = this.Servers,
            Tags = this.Tags
            )

    let createDefaultFromPaths paths =
        OpenApiDocument(
            Info = Microsoft.OpenApi.Models.OpenApiInfo(Version = "1.0.0", Title = "Swagger Petstore (Simple)"),
            Servers = ResizeArray([ Microsoft.OpenApi.Models.OpenApiServer(Url = "https://localhost:5001") ]),
            Paths = paths
        )

    let createFromRecord (record: T) =
        let servers = 
            record.ServerUrls
            |> Array.map (fun x -> Microsoft.OpenApi.Models.OpenApiServer(Url = x))
        OpenApiDocument(
            Info = Microsoft.OpenApi.Models.OpenApiInfo(Version = "1.0.0", Title = "Swagger Petstore (Simple)"),
            Servers = servers,
            Paths = record.Paths,
            Components = Microsoft.OpenApi.Models.OpenApiComponents(Schemas = record.SchemaRepo.Schemas)
        )

    let toBytes (document: OpenApiDocument) =
        use ms = new MemoryStream()
        use sw = new StreamWriter(ms)
        let writer = OpenApiJsonWriter(sw)
        document.SerializeAsV3(writer)
        sw.Flush()
        ms.ToArray()

    type OpenApiDocument with
        static member CreateDefaultFromPaths = createDefaultFromPaths
        static member CreateFromRecord = createFromRecord
        member this.WithPaths = withPaths this 
        member this.ToBytes = toBytes this 

