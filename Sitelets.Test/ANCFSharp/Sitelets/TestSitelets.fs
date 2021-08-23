module TestSitelets
    open Sitelets

    type Endpoint =
        | [<EndPoint "/sitelets">] Text1
        | [<EndPoint "/sitelets2">] Text2
        | Text3

    //let shiftedSitelet = Sitelet.Shift "shifted" sampleSitelet
    let helloWorldSite = Sitelet.Content "/hello" Endpoint.Text3 (fun _ -> box "Hello World")

    let infer =
        Sitelet.Infer (fun ctx -> function
            | Text1 ->
                box <| ctx.Link Endpoint.Text1
            | Text2 ->
                box <| ctx.Link Endpoint.Text2
        )


    let main =
        Sitelet.Sum [
            helloWorldSite
            infer
        ]

