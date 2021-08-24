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

module UnitTestHelpers =
    let helloWorldSitelet =
        Sitelet.Content "/test1" TestEndPoint.Ep1 (fun ctx -> box "Hello World")

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
let ``Hello World shifting test`` () =
    let shiftedSite = TH.helloWorldSitelet.Shift "shifted"
    let req = TH.sampleHttpRequest ()
    let link = shiftedSite.Router.Link(TestEndPoint.Ep1)
    link.Value.ToString() |> should equal "/shifted/test1"
    req.Path <- PathString("/shifted/test1")
    let routedReq = shiftedSite.Router.Route req
    routedReq |> should equal (Some TestEndPoint.Ep1)