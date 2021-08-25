module TestSitelets
    open Sitelets

    type RecTest =
        {
            A : string
            B : int
            C : bool
        }
    
    type JsonEndPoint =
        | [<EndPoint "POST /json">] [<Json "record">] JsonEP of record: RecTest

    type Endpoint =
        | [<EndPoint "/sitelets">] Text1
        | [<EndPoint "/sitelets2">] Text2
        | Text3
        | [<EndPoint "/a/s/d">] LongUrl
        | Protected
        | Login
        | Mapped

    type SubEndPoint =
        | [<EndPoint "/sub/act1">] Action1
        | [<EndPoint "/sub/act2">] Action2
        | [<EndPoint "/sub/act3">] Action3

    type SubEndPoint2 =
        | [<EndPoint "/sub/sampleep">] SampleEp

    type HasSubEndPoint =
        | Sub1 of SubEndPoint
        | Sub2 of SubEndPoint2

    type FolderEndPoint =
        | [<EndPoint "/fep1">] FEP1
        | [<EndPoint "/fep2">] FEP2

    let helloWorldSite = Sitelet.Content "/hello" Endpoint.Text3 (fun _ -> box "Hello World")

    let unmappedSite = Sitelet.Content "/mapped" Endpoint.Mapped (fun _ -> box "Hello there")

    let mappedSite = Sitelet.MapContent (fun _ -> box "Hello again") unmappedSite

    let shiftedSitelet = Sitelet.Shift "shifted" helloWorldSite

    let loginSite = Sitelet.Content "/login" Endpoint.Login (fun _ -> box "<div>Login <a href=\"https://www.google.hu/\" target=\"_blank\">here</a></div>")

    let filter: Sitelet.Filter<Endpoint> =
        {
            VerifyUser = fun _ -> true
            LoginRedirect = fun _ -> Endpoint.Login
        }

    let protectedSite = Sitelet.Protect filter <| Sitelet.Content "/protected" Endpoint.Protected (fun _ -> box "You are logged in!")

    let jsonSite =
        Sitelet.Infer (fun ctx -> function
            | JsonEP o -> box o
        )

    let subEPSite = Sitelet.Infer (fun ctx -> function
        | Action1 -> box "Act1"
        | Action2 -> box "Act2"
        | Action3 -> box "Act3"
    )

    let fep1Site = Sitelet.Content "/fep1" FolderEndPoint.FEP1 (fun _ -> box "folder endpoint 1")
    let fep2Site = Sitelet.Content "/fep2" FolderEndPoint.FEP2 (fun _ -> box "folder endpoint 2")

    let folder = [
        fep1Site
        fep2Site
    ]

    let infer =
        Sitelet.Infer (fun ctx -> function
            | Text1 ->
                box <| ctx.Link Endpoint.Text1
            | Text2 ->
                box <| ctx.Link Endpoint.Text2
            | LongUrl ->
                box <| ctx.Link Endpoint.LongUrl
        )

    let main =
        Sitelet.Sum [
            helloWorldSite
            shiftedSitelet
            loginSite
            protectedSite
            mappedSite
            infer
        ]

