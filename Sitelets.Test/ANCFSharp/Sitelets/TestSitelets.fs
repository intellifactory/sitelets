module TestSitelets
    open Sitelets

    type Endpoint =
        | Text

    let sampleSitelet = Sitelet.Content "/sitelets" Endpoint.Text (fun ctx -> box "Hello World")