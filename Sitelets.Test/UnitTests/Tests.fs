module UnitTests

open NUnit.Framework
open FsUnit
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Mvc.Testing
open Sitelets

[<SetUp>]
let Setup () =
    ()

type TestEndPoint =
    | Ep1
    | Ep2

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

[<Test>]
let ``Hello World routing test`` () =
    let req = TH.sampleHttpRequest ()

    TH.helloWorldSitelet.Router.Route req
    |> should equal (Some TestEndPoint.Ep1)
    
    req.Path <- PathString("/test2")
    TH.helloWorldSitelet.Router.Route req
    |> should equal None

[<Test>]
let ``Hello World linking test`` () =
    let link = TH.helloWorldSitelet.Router.Link(TestEndPoint.Ep1)
    link |> should be (ofCase <@ Some @>)

    link.Value.ToString() |> should equal "/test1"

    let badlink = TH.helloWorldSitelet.Router.Link(TestEndPoint.Ep2)
    badlink |> should equal None

[<Test>]
let ``Shifting test`` () =
    let shiftedSite = TH.helloWorldSitelet.Shift "shifted"
    let req = TH.sampleHttpRequest ()
    let link = shiftedSite.Router.Link(TestEndPoint.Ep1)
    link.Value.ToString() |> should equal "/shifted/test1"
    req.Path <- PathString("/shifted/test1")
    let routedReq = shiftedSite.Router.Route req
    routedReq |> should equal (Some TestEndPoint.Ep1)

    let furtherShiftedSite = shiftedSite.Shift "extrashift"
    let req = TH.sampleHttpRequest ()
    let link = furtherShiftedSite.Router.Link(TestEndPoint.Ep1)
    link.Value.ToString() |> should equal "/extrashift/shifted/test1"
    req.Path <- PathString("/extrashift/shifted/test1")
    let fortherRoutedReq = furtherShiftedSite.Router.Route req
    fortherRoutedReq |> should equal (Some TestEndPoint.Ep1)

[<Test>]
let ``Infer test`` () =
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
    inferSitelet.Router.Route req |> should equal (Some TestInferEndpoint.E1)
    req.Path <- PathString "/infer2"
    inferSitelet.Router.Route req |> should equal (Some TestInferEndpoint.E2)
    req.Path <- PathString "/infer3"
    inferSitelet.Router.Route req |> should equal (Some TestInferEndpoint.E3)

[<Test>]
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
    summedSitelet.Router.Route req
    |> should equal (Some TestEndPoint.Ep1)
    req.Path <- PathString "/test2"
    summedSitelet.Router.Route req
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
    shiftedSum.Router.Route req
    |> should equal (Some TestEndPoint.Ep1)
    req.Path <- PathString "/test1"
    shiftedSum.Router.Route req
    |> should equal (Some TestEndPoint.Ep1)
    req.Path <- PathString "/test2"
    shiftedSum.Router.Route req
    |> should equal (Some TestEndPoint.Ep2)
    req.Path <- PathString "/shifted/test2"
    shiftedSum.Router.Route req
    |> should equal None

//let ``MapContent test`` () =
//    let sitelet = TH.helloWorldSitelet
//    let contentMappedSitelet =
//        Sitelet.MapContent (fun _ -> box )