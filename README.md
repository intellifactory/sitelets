# IntelliFactory.Sitelets

`Sitelets` is an F#-based library, that provides abstractions for creating ASP.NET Core web applications.

## Table of contents

* [Installing](#installing)
* [Using Sitelets](#using-sitelets)
* [Working with other ANC web frameworks](#working-with-other-anc-web-frameworks)
* [Contributing](#contributing)

## Installing

The easiest way to add Sitelets to your project is by using dotnet cli:

```
dotnet add YOUR_PROJECTFILE package Sitelets
```

or if you work in Visual Studio, search for Sitelets in Nuget and install it in your project!

## Using Sitelets

For example let's create an ASP.NET Core Mvc app and plug Sitelets into it to make a Hello World site:

```
dotnet new mvc -lang f# -o MyMvcApp

dotnet add MyMvcApp/MyMvcApp.fsproj package Sitelets

```

For clarity, let's add a `MySitelets` folder in to project and create a `HelloWorld.fs` source file in it. Now for this site we only want to print the 'Hello World' text onto the page, only this much code will do the job:

```fsharp
module Hello

    open Sitelets

    type EndPoint =
    | HelloEndPoint

    let helloWorldSite = Sitelet.Content "/hello" EndPoint.HelloEndPoint (fun _ -> box "Hello World")
```

We also have to plug the sitelet into the application pipeline. Go to `Startup.fs` and do this:

```fsharp
...

app.UseAuthorization() |> ignore

// plug sitelet in here for example
app.UseSitelets(Hello.helloWorldSite) |> ignore

app.UseEndpoints(fun endpoints ->
...
```

After a quick build and run, if you go to the `/hello` endpoint, you will see "Hello World" printed there.

## Working with other ANC web frameworks

Sitelets is a layer of abstraction on top of ASP.NET Core so it can work with other F# web frameworks like Giraffe or Saturn for example. Let's take a look at a Giraffe app, for starters:

```
dotnet new giraffe -o MyGiraffeApp

dotnet add MyGiraffeApp/MyGiraffeApp.fsproj package Sitelets
```

Let's add a sitelet that echoes what you write after the `/echo` endpoint

```fsharp
// ---------------------------------
// Sitelet
// ---------------------------------

type Endpoint =
    | [<EndPoint "GET /echo">] Str of string

let inferredSite =
    Sitelet.Infer (fun ctx -> function
        | Str str -> box str
    )

...

let webApp =
    choose [
        GET >=>
            choose [
                route "/" >=> indexHandler "world"
                routef "/hello/%s" indexHandler
                SiteletHelper.sitelet inferredSite
            ]
        setStatusCode 404 >=> text "Not Found" ]
```

Add the sitelet to the pipeline and we are done!

## Sitelets in C#

Although the focus is on the F# side, you can also use the Sitelets library in C# ASP.NET Core project. As an example:

```cs
public class TestSitelet
    {
        public static Sitelet<object> S =>
        new SiteletBuilder()
            .With("/hello", ctx =>
                    "Hello World from C#"
            )
            .Install();
    }
```

## Contributing

If you find any faults, please [submit a ticket](https://github.com/intellifactory/sitelets/issues/4).

It is an open source project so anyone is more than welcome to contribute but make sure to discuss the changes before opening a PR, otherwise it might get rejected.

## Links

* [Nuget](#TODO)
* [Intellifactory](https://intellifactory.com/)