module UnitTests

open NUnit.Framework
open FsUnit
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Mvc.Testing
open Sitelets
open TestSitelets

[<SetUp>]
let Setup () =
    ()

type TestEndPoint =
    | Ep1
    | Ep2

type TestEndPoint2 =
    | [<EndPoint "/mappedendpoint1">] Ep1
    | [<EndPoint "/mappedendpoint2">] Ep2

type TestInferEndpoint =
    | [<EndPoint "/infer1">] E1
    | [<EndPoint "/infer2">] E2
    | [<EndPoint "/infer3">] E3

module UnitTestHelpers =
    let helloWorldSitelet =
        Sitelet.Content "/test1" TestEndPoint.Ep1 (fun ctx -> box "Hello World")

    let helloWorldSitelet2 =
        Sitelet.Content "/test2" TestEndPoint.Ep2 (fun ctx -> box "Hello, World!")

    let sampleHttpRequest() =
        let ctx = DefaultHttpContext()
        let httpReq = ctx.Request
        httpReq.Scheme <- "http"
        httpReq.Method <- "GET"
        httpReq.Host <- HostString("localhost:5000")
        httpReq.Path <- PathString("/test1")
        httpReq

module TH = UnitTestHelpers

[<Test; Category("Hello World Tests")>]
let ``Hello World routing Test`` () =
    let req = TH.sampleHttpRequest ()

    TH.helloWorldSitelet.Router.Route <| RoutedHttpRequest req
    |> should equal (Some TestEndPoint.Ep1)
    
    req.Path <- PathString("/test2")
    TH.helloWorldSitelet.Router.Route <| RoutedHttpRequest req
    |> should equal None

[<Test; Category("Hello World Tests")>]
let ``Hello World linking Test`` () =
    let link = TH.helloWorldSitelet.Router.Link(TestEndPoint.Ep1)
    link |> should be (ofCase <@ Some @>)

    link.Value.ToString() |> should equal "/test1"

    let badlink = TH.helloWorldSitelet.Router.Link(TestEndPoint.Ep2)
    badlink |> should equal None

[<Test; Category("Sitelet Tests")>]
let ``Shifting Test`` () =
    let shiftedSite = TH.helloWorldSitelet.Shift "shifted"
    let req = TH.sampleHttpRequest ()
    let link = shiftedSite.Router.Link(TestEndPoint.Ep1)
    link.Value.ToString() |> should equal "/shifted/test1"
    req.Path <- PathString("/shifted/test1")
    let routedReq = shiftedSite.Router.Route <| RoutedHttpRequest req
    routedReq |> should equal (Some TestEndPoint.Ep1)

    let furtherShiftedSite = shiftedSite.Shift "extrashift"
    let req = TH.sampleHttpRequest ()
    let link = furtherShiftedSite.Router.Link(TestEndPoint.Ep1)
    link.Value.ToString() |> should equal "/extrashift/shifted/test1"
    req.Path <- PathString("/extrashift/shifted/test1")
    let fortherRoutedReq = furtherShiftedSite.Router.Route <| RoutedHttpRequest req
    fortherRoutedReq |> should equal (Some TestEndPoint.Ep1)

[<Test; Category("Sitelet Tests")>]
let ``Infer Test`` () =
    let inferSitelet =
        Sitelet.Infer (fun ctx -> function
            | E1 -> box "Infer endpoint 1"
            | E2 -> box "Infer endpoint 2"
            | E3 -> box "Infer endpoint 3"
        )
    let link1 = inferSitelet.Router.Link TestInferEndpoint.E1
    link1.Value.ToString() |> should equal "/infer1"
    let link2 = inferSitelet.Router.Link TestInferEndpoint.E2
    link2.Value.ToString() |> should equal "/infer2"
    let link3 = inferSitelet.Router.Link TestInferEndpoint.E3
    link3.Value.ToString() |> should equal "/infer3"

    let req = TH.sampleHttpRequest ()
    req.Path <- PathString "/infer1"
    inferSitelet.Router.Route <| RoutedHttpRequest req |> should equal (Some TestInferEndpoint.E1)
    req.Path <- PathString "/infer2"
    inferSitelet.Router.Route <| RoutedHttpRequest req |> should equal (Some TestInferEndpoint.E2)
    req.Path <- PathString "/infer3"
    inferSitelet.Router.Route <| RoutedHttpRequest req |> should equal (Some TestInferEndpoint.E3)

[<Test; Category("Sitelet Tests")>]
let ``Sum Test`` () =
    let summedSitelet =
        Sitelet.Sum [
            TH.helloWorldSitelet
            TH.helloWorldSitelet2
        ]
    let link1 = summedSitelet.Router.Link TestEndPoint.Ep1
    link1.Value.ToString() |> should equal "/test1"
    let link2 = summedSitelet.Router.Link TestEndPoint.Ep2
    link2.Value.ToString() |> should equal "/test2"
    let req = TH.sampleHttpRequest ()
    summedSitelet.Router.Route <| RoutedHttpRequest req
    |> should equal (Some TestEndPoint.Ep1)
    req.Path <- PathString "/test2"
    summedSitelet.Router.Route <| RoutedHttpRequest req
    |> should equal (Some TestEndPoint.Ep2)

    let shiftedHelloWorldSite =
        TH.helloWorldSitelet.Shift "shifted"
    let req2 = TH.sampleHttpRequest ()
    let shiftedSum = 
        Sitelet.Sum [
            shiftedHelloWorldSite
            TH.helloWorldSitelet
            TH.helloWorldSitelet2
        ]
    req.Path <- PathString "/shifted/test1"
    shiftedSum.Router.Route <| RoutedHttpRequest req
    |> should equal (Some TestEndPoint.Ep1)
    req.Path <- PathString "/test1"
    shiftedSum.Router.Route <| RoutedHttpRequest req
    |> should equal (Some TestEndPoint.Ep1)
    req.Path <- PathString "/test2"
    shiftedSum.Router.Route <| RoutedHttpRequest req
    |> should equal (Some TestEndPoint.Ep2)
    req.Path <- PathString "/shifted/test2"
    shiftedSum.Router.Route <| RoutedHttpRequest req
    |> should equal None

[<Test; Category("Sitelet Tests")>]
let ``EmbedInUnion Test`` () =
    let sitelet = Sitelet.EmbedInUnion <@ HasSubEndPoint.Sub1 @> subEPSite
    let link1 = sitelet.Router.Link (HasSubEndPoint.Sub1 <| SubEndPoint.Action1)
    link1 |> should be (ofCase <@ Some @>)
    link1.Value.ToString() |> should equal "/sub/act1"
    let link2 = sitelet.Router.Link (HasSubEndPoint.Sub1 <| SubEndPoint.Action2)
    link2 |> should be (ofCase <@ Some @>)
    link2.Value.ToString() |> should equal "/sub/act2"
    let link3 = sitelet.Router.Link (HasSubEndPoint.Sub1 <| SubEndPoint.Action3)
    link3 |> should be (ofCase <@ Some @>)
    link3.Value.ToString() |> should equal "/sub/act3"

    let req = TH.sampleHttpRequest ()
    req.Path <- PathString "/sub/act1"
    sitelet.Router.Route <| RoutedHttpRequest req
    |> should equal (Some <| HasSubEndPoint.Sub1 SubEndPoint.Action1)
    req.Path <- PathString "/sub/act2"
    sitelet.Router.Route <| RoutedHttpRequest req
    |> should equal (Some <| HasSubEndPoint.Sub1 SubEndPoint.Action2)
    req.Path <- PathString "/sub/act3"
    sitelet.Router.Route <| RoutedHttpRequest req
    |> should equal (Some <| HasSubEndPoint.Sub1 SubEndPoint.Action3)

[<Test; Category("Sitelet Tests")>]
let ``InferPartialInUnion Test`` () =
    let sitelet = Sitelet.InferPartialInUnion <@ HasSubEndPoint.Sub2 @> (fun ctx ep -> box "Yes")
    let link = sitelet.Router.Link (HasSubEndPoint.Sub2 <| SubEndPoint2.SampleEp)
    link |> should be (ofCase <@ Some @>)
    link.Value.ToString() |> should equal "/sub/sampleep"

    let req = TH.sampleHttpRequest ()
    req.Path <- PathString "/sub/sampleep"
    sitelet.Router.Route <| RoutedHttpRequest req
    |> should equal (Some <| HasSubEndPoint.Sub2 SubEndPoint2.SampleEp)
    req.Path <- PathString "/sub/act1"
    sitelet.Router.Route <| RoutedHttpRequest req
    |> should equal None

//[<Test; Category("Sitelet Tests")>]
//let ``Empty Test`` () =
//    let sitelet = Sitelet.Empty<string>
//    sitelet.Router.Link "" |> should equal null

[<Test; Category("Sitelet Tests")>]
let ``Folder Test`` () =
    let sitelet = Sitelet.Folder "/folder" folder
    let link1 = sitelet.Router.Link FolderEndPoint.FEP1
    link1.Value.ToString() |> should equal "/folder/fep1"
    let link2 = sitelet.Router.Link FolderEndPoint.FEP2
    link2.Value.ToString() |> should equal "/folder/fep2"

    let req = TH.sampleHttpRequest ()
    req.Path <- PathString "/folder/fep1"
    sitelet.Router.Route <| RoutedHttpRequest req
    |> should equal (Some FolderEndPoint.FEP1)
    req.Path <- PathString "/folder/fep2"
    sitelet.Router.Route <| RoutedHttpRequest req
    |> should equal (Some FolderEndPoint.FEP2)
    req.Path <- PathString "/fep1"
    sitelet.Router.Route <| RoutedHttpRequest req
    |> should equal None
    req.Path <- PathString "/fep2"
    sitelet.Router.Route <| RoutedHttpRequest req
    |> should equal None

[<Test; Category("Sitelet Tests")>]
let ``New Test`` () =
    let newSitelet = Sitelet.New TH.helloWorldSitelet.Router TH.helloWorldSitelet.Controller
    let link = newSitelet.Router.Link TestEndPoint.Ep1
    link |> should be (ofCase <@ Some @>)
    link.Value.ToString() |> should equal "/test1"

    newSitelet.Router.Route <| (RoutedHttpRequest <| TH.sampleHttpRequest ())
    |> should equal <| Some TestEndPoint.Ep1
    let req = TH.sampleHttpRequest ()
    req.Path <- PathString "/test2"
    newSitelet.Router.Route <| RoutedHttpRequest req
    |> should equal <| None

[<Test; Category("Sitelet Tests")>]
let ``Box/Unbox Test`` () =
    // box
    let boxedSite = Sitelet.Box TH.helloWorldSitelet
    let link = boxedSite.Router.Link <| box TestEndPoint.Ep1
    link |> should be (ofCase <@ Some @>)
    link.Value.ToString() |> should equal "/test1"

    boxedSite.Router.Route <| (RoutedHttpRequest <| TH.sampleHttpRequest ())
    |> should equal <| (Some <| box TestEndPoint.Ep1)

    // unbox
    let unboxedSite = Sitelet.Unbox boxedSite
    let link = unboxedSite.Router.Link TestEndPoint.Ep1
    link |> should be (ofCase <@ Some @>)
    link.Value.ToString() |> should equal "/test1"

    unboxedSite.Router.Route <| (RoutedHttpRequest <| TH.sampleHttpRequest ())
    |> should equal <| Some TestEndPoint.Ep1

[<Test; Category("Sitelet Tests")>]
let ``Map Test`` () =
    let sitelet = Sitelet.Map (fun _ -> TestEndPoint2.Ep1) (fun _ -> TestEndPoint.Ep1) TH.helloWorldSitelet
    let link1 = sitelet.Router.Link TestEndPoint2.Ep1
    link1.Value.ToString() |> should equal "/test1"

    let req = TH.sampleHttpRequest ()
    req.Path <- PathString "/test1"
    sitelet.Router.Route <| RoutedHttpRequest req
    |> should equal (Some TestEndPoint2.Ep1)

[<Test; Category("Sitelet Tests")>]
let ``TryMap Test`` () =
    let sitelet = Sitelet.TryMap (fun _ -> Some TestEndPoint2.Ep1) (fun _ -> Some TestEndPoint.Ep1) TH.helloWorldSitelet
    let link1 = sitelet.Router.Link TestEndPoint2.Ep1
    link1.Value.ToString() |> should equal "/test1"

    let req = TH.sampleHttpRequest ()
    req.Path <- PathString "/test1"
    sitelet.Router.Route <| RoutedHttpRequest req
    |> should equal (Some TestEndPoint2.Ep1)

    let badsitelet = Sitelet.TryMap (fun _ -> None) (fun _ -> None) TH.helloWorldSitelet
    let link1 = badsitelet.Router.Link TestEndPoint2.Ep1
    link1 |> should be null

    let req = TH.sampleHttpRequest ()
    req.Path <- PathString "/test1"
    badsitelet.Router.Route <| RoutedHttpRequest req
    |> should equal None

[<Test; Category("Sitelet Tests")>]
let ``Embed Test`` () =
    let sitelet = Sitelet.Embed (fun _ -> BasicEP.BasicEp1) (fun _ -> Some TestEndPoint.Ep1) TH.helloWorldSitelet
    let link = sitelet.Router.Link BasicEP.BasicEp1
    link |> should be (ofCase <@ Some @>)
    link.Value.ToString() |> should equal "/test1"

    let req = TH.sampleHttpRequest ()
    sitelet.Router.Route <| RoutedHttpRequest req
    |> should equal (Some BasicEP.BasicEp1)

//[<Test; Category("Sitelet Tests")>]
//let ``InferWithCustomErrors Test`` () =
//    let sitelet = Sitelet.InferWithCustomErrors (fun (ctx: Context<BasicEP>) (a: ParseRequestResult<BasicEP>) -> box "custom error infer")
//    let link = sitelet.Router.Link <| ParseRequestResult.Success BasicEP.BasicEp1
//    link |> should be (ofCase <@ Some @>)
    
[<Test; Category("Sitelet Tests")>]
let ``InferPartial Test`` () =
    let sitelet = Sitelet.InferPartial (fun _ -> TestEndPoint.Ep1) (fun _ -> Some <| HasSubEndPoint.Sub1 SubEndPoint.Action1) (fun ctx a -> box "asd")
    let link1 = sitelet.Router.Link <| TestEndPoint.Ep1
    link1 |> should be (ofCase <@ Some @>)
    link1.Value.ToString() |> should equal "/Sub1/sub/act1"

    let req = TH.sampleHttpRequest ()
    req.Path <- PathString "/Sub1/sub/act1"
    sitelet.Router.Route <| RoutedHttpRequest req
    |> should equal (Some TestEndPoint.Ep1)