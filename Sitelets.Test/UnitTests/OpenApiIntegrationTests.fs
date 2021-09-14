module OpenApiIntegrationTests

open NUnit.Framework
open FsUnit
open Sitelets
open System.Text.Json
open System.Text.Json.Serialization
open OpenApiDocument
open OpenApiPaths
open OpenApiPathItem
open OpenApiOperation
open OpenApiSchema
open OpenApiResponses
open System

[<SetUp>]
let Setup () =
    ()

type Endpoint =
    | [<EndPoint "GET /echo">] Str of msg:string * value:int * rate:float 

type QueryParameteredEndpoint =
    | [<EndPoint "GET /echo">] [<Query("value")>] Str of msg:string * value:int * rate:float 

[<EndPoint "POST /echo">] 
type TestRecord = {Msg:string; Value:int; Rate:float}

type GiraffeEndpoint =
    | [<EndPoint "GET /echo">] Str of string
    | [<EndPoint "GET /actionresult">] ActionResult
    | [<EndPoint "GET /task/str">] Task1
    | [<EndPoint "GET /task/actionresult">] Task2
    | [<EndPoint "GET /task/enumerable">] Task3
    | [<EndPoint "GET /enum">] Enumerable1
    | [<EndPoint "GET /enum/async">] Enumerable2
    | Test1
    | Test2

type DummyJsonResponse =
    {
        Id : int
        Name : string
        PhotoUrls : string array
    }

type UnnamedParametersEndpoints =
    | [<EndPoint "GET /strings">] Str of string * string
    | [<EndPoint "GET /ints">] Int of int * int

let serializerOptions = 
    let options = JsonSerializerOptions(PropertyNamingPolicy=JsonNamingPolicy.CamelCase)
    options.Converters.Add( JsonStringEnumConverter())
    options.Converters.Add(
        JsonFSharpConverter(
            JsonUnionEncoding.ExternalTag
            ||| JsonUnionEncoding.NamedFields
            ||| JsonUnionEncoding.UnwrapFieldlessTags
            ||| JsonUnionEncoding.UnwrapOption))
    options

[<Test; Category("OpenApi Integration Tests")>]
let ``pascal case works`` () =
    let info = System.Globalization.CultureInfo.InvariantCulture.TextInfo

    let pascalCase (s: string) =
        s
        |> (info.ToLower >> info.ToTitleCase)

    pascalCase "GET"
    |> should equal "Get"

[<Test; Category("OpenApi Integration Tests")>]
let ``test schema equality`` () =
    let schema = OpenApiSchema.OpenApiSchema(Deprecated = true)
    schema |> should equal (OpenApiSchema.OpenApiSchema(Deprecated = true))

[<Test; Category("OpenApi Integration Tests")>]
let ``test parameter equality`` () =
    let schema = OpenApiSchema.OpenApiSchema(Deprecated = true)

    OpenApiParameter.createDefaultFromSchemaAndName schema "aa"
    |> should equal (OpenApiParameter.createDefaultFromSchemaAndName schema "aa")

[<Test; Category("OpenApi Integration Tests")>]
let ``test operation equality`` () =
    let schema = OpenApiSchema.OpenApiSchema(Deprecated = true)

    OpenApiOperation.createDefaultFromSchemaAndName schema "aa"
    |> should equal (OpenApiOperation.createDefaultFromSchemaAndName schema "aa")

[<Test; Category("OpenApi Integration Tests")>]
let ``test path item equality`` () =
    let schema = OpenApiSchema.OpenApiSchema(Deprecated = true)
    let operation = OpenApiOperation.createDefaultFromSchemaAndName schema "aa"

    OpenApiPathItem.createFromRecord {
            Description = "desc"
            Extensions = Option.toObj None 
            Parameters = Option.toObj None
            Servers = Option.toObj None
            Summary = "summ"
            Operations = dict [ (Microsoft.OpenApi.Models.OperationType.Get, operation :> Microsoft.OpenApi.Models.OpenApiOperation) ]
    }
    |> should equal (OpenApiPathItem.createFromRecord {
            Description = "desc"
            Extensions = Option.toObj None 
            Parameters = Option.toObj None
            Servers = Option.toObj None
            Summary = "summ"
            Operations = dict [ (Microsoft.OpenApi.Models.OperationType.Get, operation :> Microsoft.OpenApi.Models.OpenApiOperation) ]
    })

[<Test; Category("OpenApi Integration Tests")>]
let ``test paths equality`` () =
    let schema = OpenApiSchema.OpenApiSchema(Deprecated = true)
    let operation = OpenApiOperation.createDefaultFromSchemaAndName schema "aa"

    let pathItem = OpenApiPathItem.createFromRecord {
            Description = "desc"
            Extensions = Option.toObj None 
            Parameters = Option.toObj None
            Servers = Option.toObj None
            Summary = "summ"
            Operations = dict [ (Microsoft.OpenApi.Models.OperationType.Get, operation :> Microsoft.OpenApi.Models.OpenApiOperation) ]
    }
    OpenApiPaths.createFromPathPathItemMapping <| dict [ ("path", pathItem) ]
    |> should equal (OpenApiPaths.createFromPathPathItemMapping <| dict [ ("path", pathItem) ])

[<Test; Category("OpenApi Integration Tests")>]
let ``test document equality`` () =
    let schema = OpenApiSchema.OpenApiSchema(Deprecated = true)
    let operation = OpenApiOperation.createDefaultFromSchemaAndName schema "aa"

    let pathItem = OpenApiPathItem.createFromRecord {
            Description = "desc"
            Extensions = Option.toObj None 
            Parameters = Option.toObj None
            Servers = Option.toObj None
            Summary = "summ"
            Operations = dict [ (Microsoft.OpenApi.Models.OperationType.Get, operation :> Microsoft.OpenApi.Models.OpenApiOperation) ]
    }
    let paths = OpenApiPaths.createFromPathPathItemMapping <| dict [ ("path", pathItem) ]

    OpenApiDocument.createDefaultFromPaths paths
    |> should equal (OpenApiDocument.createDefaultFromPaths paths)

let testType t expected =
    match OpenApiIntegration.generateOpenApiDefault serializerOptions t with
    | Ok document ->
        document |> should equal expected
    | Error (schemaRepo, pathsOption, _) ->
        match pathsOption with
        | Some paths ->
            let document = OpenApiDocument.createDefaultFromPaths paths
            document.Components <- Microsoft.OpenApi.Models.OpenApiComponents(Schemas = schemaRepo.Schemas)
            document |> should equal expected
        | None ->
            failwith "None of the endpoints' type were implemented"

[<Test; Category("OpenApi Integration Tests")>]
let ``Schema as expected for 3 union case`` () =
    let openApiSchemaGenerator =
        createDefaultSchemaGeneratorFromSerializerOptions serializerOptions
    let schemaRepo = Swashbuckle.AspNetCore.SwaggerGen.SchemaRepository()
    let generateSchema (ty : Type) = 
        let schema = openApiSchemaGenerator.GenerateSchema(ty, schemaRepo)
        let ret = OpenApiSchema()
        OpenApiSchema.copyJsonSchemaProperties schema ret
        ret :> Microsoft.OpenApi.Models.OpenApiSchema

    let schema1 = generateSchema typeof<string>
    let schema2 = generateSchema typeof<int>
    let schema3 = generateSchema typeof<float>
    let operations = 
        let defaultOperations =
            OpenApiOperation.createDefaultFromSchemaAndName schema1 "dummy"
        withParameters defaultOperations [|
            OpenApiParameter.createDefaultFromSchemaAndName schema1 "msg"
            OpenApiParameter.createDefaultFromSchemaAndName schema2 "value"
            OpenApiParameter.createDefaultFromSchemaAndName schema3 "rate"
        |]
    let getOperation = dict [ (Microsoft.OpenApi.Models.OperationType.Get, operations) ]
    let expected = createDefaultFromPaths <| (OpenApiPaths.createFromPathPathItemMapping <| dict [
            ("/echo/{msg}/{value}/{rate}", withOperations (OpenApiPathItem()) getOperation)
        ])
    expected.Components <- Microsoft.OpenApi.Models.OpenApiComponents(Schemas = schemaRepo.Schemas)

    testType typeof<Endpoint> expected

[<Test; Category("OpenApi Integration Tests")>]
let ``Unnamed parameters`` () =
    let openApiSchemaGenerator =
        createDefaultSchemaGeneratorFromSerializerOptions serializerOptions
    let schemaRepo = Swashbuckle.AspNetCore.SwaggerGen.SchemaRepository()
    let generateSchema (ty : Type) = 
        let schema = openApiSchemaGenerator.GenerateSchema(ty, schemaRepo)
        let ret = OpenApiSchema()
        OpenApiSchema.copyJsonSchemaProperties schema ret
        ret :> Microsoft.OpenApi.Models.OpenApiSchema

    let schema1 = generateSchema typeof<string>
    let schema2 = generateSchema typeof<int>
    let defaultOperations =
        OpenApiOperation.createDefaultFromSchemaAndName schema1 "dummy"
    let stringOperations = 
         withParameters defaultOperations [|
            OpenApiParameter.createDefaultFromSchemaAndName schema1 "String 1"
            OpenApiParameter.createDefaultFromSchemaAndName schema1 "String 2"
         |]
    let intOperations = 
         withParameters defaultOperations [|
            OpenApiParameter.createDefaultFromSchemaAndName schema2 "Int32 1"
            OpenApiParameter.createDefaultFromSchemaAndName schema2 "Int32 2"
         |]
    let stringOperation = dict [ (Microsoft.OpenApi.Models.OperationType.Get, stringOperations) ]
    let intOperation = dict [ (Microsoft.OpenApi.Models.OperationType.Get, intOperations) ]
    let expected = createDefaultFromPaths <| (OpenApiPaths.createFromPathPathItemMapping <| dict [
            ("/strings/{String 1}/{String 2}", withOperations (OpenApiPathItem()) stringOperation)
            ("/ints/{Int32 1}/{Int32 2}", withOperations (OpenApiPathItem()) intOperation)
        ])
    expected.Components <- Microsoft.OpenApi.Models.OpenApiComponents(Schemas = schemaRepo.Schemas)

    testType typeof<UnnamedParametersEndpoints> expected

[<Test; Category("OpenApi Integration Tests")>]
let ``Query params doesn't appear in Path`` () =
    let openApiSchemaGenerator =
        createDefaultSchemaGeneratorFromSerializerOptions serializerOptions
    let schemaRepo = Swashbuckle.AspNetCore.SwaggerGen.SchemaRepository()
    let generateSchema (ty : Type) = 
        let schema = openApiSchemaGenerator.GenerateSchema(ty, schemaRepo)
        let ret = OpenApiSchema()
        OpenApiSchema.copyJsonSchemaProperties schema ret
        ret :> Microsoft.OpenApi.Models.OpenApiSchema

    let schema1 = generateSchema typeof<string>
    let schema2 = generateSchema typeof<int>
    let schema3 = generateSchema typeof<float>
    let operations = 
        let defaultOperations =
            OpenApiOperation.createDefaultFromSchemaAndName schema1 "dummy"
        let queryValueParam = 
            let valueParam = OpenApiParameter.createDefaultFromSchemaAndName schema2 "value"
            OpenApiParameter.withIn valueParam (Nullable <| Microsoft.OpenApi.Models.ParameterLocation.Query)
        withParameters defaultOperations [|
            OpenApiParameter.createDefaultFromSchemaAndName schema1 "msg"
            queryValueParam
            OpenApiParameter.createDefaultFromSchemaAndName schema3 "rate"
        |]
    let getOperation = dict [ (Microsoft.OpenApi.Models.OperationType.Get, operations) ]
    let expected = createDefaultFromPaths <| (OpenApiPaths.createFromPathPathItemMapping <| dict [
            ("/echo/{msg}/{rate}", withOperations (OpenApiPathItem()) getOperation)
        ])
    expected.Components <- Microsoft.OpenApi.Models.OpenApiComponents(Schemas = schemaRepo.Schemas)

    testType typeof<QueryParameteredEndpoint> expected

[<Test; Category("OpenApi Integration Tests")>]
let ``Giraffe Endpoint`` () =
    let openApiSchemaGenerator =
        createDefaultSchemaGeneratorFromSerializerOptions serializerOptions
    let schemaRepo = Swashbuckle.AspNetCore.SwaggerGen.SchemaRepository()
    let generateSchema (ty : Type) = 
        let schema = openApiSchemaGenerator.GenerateSchema(ty, schemaRepo)
        let ret = OpenApiSchema()
        OpenApiSchema.copyJsonSchemaProperties schema ret
        ret :> Microsoft.OpenApi.Models.OpenApiSchema

    let schema1 = generateSchema typeof<string>
    let operations = 
        OpenApiOperation.createDefaultFromSchemaAndName schema1 "String"
    let getOperation = dict [ (Microsoft.OpenApi.Models.OperationType.Get, operations) ]
    let emptyOperation = withResponses (OpenApiOperation()) OpenApiResponses.createDefault
    let emptyPathWithGet = withOperations (OpenApiPathItem()) <| dict [ (Microsoft.OpenApi.Models.OperationType.Get, emptyOperation) ]
    let expected = createDefaultFromPaths <| (OpenApiPaths.createFromPathPathItemMapping <| dict [
            ("/echo/{String}", withOperations (OpenApiPathItem()) getOperation)
            ("/actionresult", emptyPathWithGet)
            ("/task/str", emptyPathWithGet)
            ("/actionresult", emptyPathWithGet)
            ("/task/str", emptyPathWithGet)
            ("/task/actionresult", emptyPathWithGet)
            ("/task/enumerable", emptyPathWithGet)
            ("/enum", emptyPathWithGet)
            ("/enum/async", emptyPathWithGet)
            ("/Test1", emptyPathWithGet)
            ("/Test2", emptyPathWithGet)
        ])
    expected.Components <- Microsoft.OpenApi.Models.OpenApiComponents(Schemas = schemaRepo.Schemas)

    testType typeof<GiraffeEndpoint> expected

[<Test; Category("OpenApi Integration Tests")>]
let ``Check POST record`` () =

    let openApiSchemaGenerator =
        createDefaultSchemaGeneratorFromSerializerOptions serializerOptions
    let schemaRepo = Swashbuckle.AspNetCore.SwaggerGen.SchemaRepository()
    let generateSchema (ty : Type) = 
        let schema = openApiSchemaGenerator.GenerateSchema(ty, schemaRepo)
        let ret = OpenApiSchema()
        OpenApiSchema.copyJsonSchemaProperties schema ret
        ret :> Microsoft.OpenApi.Models.OpenApiSchema

    let schema1 = generateSchema typeof<string>
    let schema2 = generateSchema typeof<int>
    let schema3 = generateSchema typeof<float>
    let operations = 
        let defaultOperations =
            OpenApiOperation.createDefaultFromSchemaAndName schema1 "dummy"
        withParameters defaultOperations [|
            OpenApiParameter.createDefaultFromSchemaAndName schema1 "Msg"
            OpenApiParameter.createDefaultFromSchemaAndName schema2 "Value"
            OpenApiParameter.createDefaultFromSchemaAndName schema3 "Rate"
        |]
    let postOperation = dict [ (Microsoft.OpenApi.Models.OperationType.Post, operations) ]
    let postPathItem = withOperations (OpenApiPathItem()) postOperation
    let echoOperation = OpenApiPaths.createFromPathPathItemMapping <| dict [ ("/echo/{Msg}/{Value}/{Rate}", postPathItem) ]
    let expected = createDefaultFromPaths echoOperation
    expected.Components <- Microsoft.OpenApi.Models.OpenApiComponents(Schemas = schemaRepo.Schemas)

    testType typeof<TestRecord> expected

[<Test; Category("OpenApi Integration Tests")>]
let ``Paths as expected from raw string`` () =
    let expected = """{
  "/pet/{petId}": {
    "get": {
      "summary": "Find pet by ID",
      "parameters": [
        {
          "name": "petId",
          "in": "path",
          "required": true,
          "schema": {
            "type": "integer",
            "format": "int32"
          }
        },
        {
          "name": "otherId",
          "in": "path",
          "required": true,
          "schema": {
            "type": "integer",
            "format": "int32"
          }
        }
      ],
      "responses": {
        "200": {
          "description": null,
          "content": {
            "application/json": {
              "schema": {
                "$ref": "#/components/schemas/DummyJsonResponse"
              }
            }
          }
        }
      }
    }
  }
}"""
    let openApiSchemaGenerator =
        createDefaultSchemaGeneratorFromSerializerOptions serializerOptions
    let schemaRepo = Swashbuckle.AspNetCore.SwaggerGen.SchemaRepository()
    let generateSchema (ty : Type) = 
        let schema = openApiSchemaGenerator.GenerateSchema(ty, schemaRepo)
        let ret = OpenApiSchema()
        OpenApiSchema.copyJsonSchemaProperties schema ret
        ret :> Microsoft.OpenApi.Models.OpenApiSchema

    let response = 
        let schema = generateSchema typeof<DummyJsonResponse>
        Microsoft.OpenApi.Models.OpenApiResponse(
            Description = Option.toObj None,
            Content = dict [ "application/json", Microsoft.OpenApi.Models.OpenApiMediaType(Schema = schema) ])

    let openApiOperation =
        let schema = generateSchema typeof<int>
        
        OpenApiOperation.createFromRecord {
            Summary = "Find pet by ID"
            Description = Option.toObj None
            Parameters = [
                OpenApiParameter.createDefaultFromSchemaAndName schema "petId"
                OpenApiParameter.createDefaultFromSchemaAndName schema "otherId"
                ] |> ResizeArray
            RequestBody = Option.toObj None
            Responses = createDefaultFromResponses <| dict [ ("200", response) ]
        }
    let openApiPathItem = 
        withOperations (OpenApiPathItem()) (dict [ (Microsoft.OpenApi.Models.OperationType.Get, openApiOperation) ])
    createFromPathPathItemMapping <| dict [ ("/pet/{petId}", openApiPathItem) ] 
    |> string
    |> (fun x -> x.Replace("\n", "\r\n"))
    |> should equal expected
