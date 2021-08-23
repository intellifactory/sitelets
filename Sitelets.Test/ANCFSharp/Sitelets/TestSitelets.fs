module TestSitelets
    open Sitelets

    type Endpoint =
        | [<EndPoint "/sitelets">] Text1
        | [<EndPoint "/sitelets2">] Text2
        | [<EndPoint "hello">] Text3
        | [<EndPoint "/a/s/d">] LongUrl
        | Protected
        | Login

    let helloWorldSite = Sitelet.Content "/hello" Endpoint.Text3 (fun _ -> box "Hello World")

    let shiftedSitelet = Sitelet.Shift "shifted" helloWorldSite

    let loginSite = Sitelet.Content "/login" Endpoint.Login (fun _ -> box "<div>Login <a href=\"https://www.google.hu/\" target=\"_blank\">here</a></div>")

    let filter: Sitelet.Filter<Endpoint> =
        {
            VerifyUser = fun _ -> true
            LoginRedirect = fun _ -> Endpoint.Login
        }

    let protectedSite = Sitelet.Protect filter <| Sitelet.Content "/protected" Endpoint.Protected (fun _ -> box "You are logged in!")

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
            infer
        ]

