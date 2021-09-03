module AuthenticationHandlerModel

    type TokenModel =
        {
            UserId: int
            Name: string
            EmailAddress: string
        }

    open System
    open System.IO
    open System.Security.Claims
    open System.Text
    open System.Text.Encodings.Web
    open System.Threading.Tasks
    open Microsoft.AspNetCore.Authentication
    open Microsoft.Extensions.Logging
    open Microsoft.Extensions.Options

    type ValidateHashAuthenticationSchemeOptions () =
        inherit AuthenticationSchemeOptions ()

    type ValidateHashAuthenticationHandler (options, logger, encoder, clock) =
        inherit AuthenticationHandler<ValidateHashAuthenticationSchemeOptions> (options, logger, encoder, clock)

        override x.HandleAuthenticateAsync() : Task<AuthenticateResult> = 
            if not <| x.Request.Headers.ContainsKey "X-base-Token" then
                Task.FromResult <| AuthenticateResult.Fail "Header not found"
            else
                let token = x.Request.Headers.["X-base-Token"]
                try
                    let claims = [
                        Claim(ClaimTypes.NameIdentifier, token.[0])
                        Claim(ClaimTypes.Email, token.[1])
                        Claim(ClaimTypes.Name, token.[2])
                    ]
                    let claimsIdentity = new ClaimsIdentity(claims, nameof(ValidateHashAuthenticationHandler))
                    let ticket = new AuthenticationTicket(new ClaimsPrincipal(claimsIdentity), x.Scheme.Name)
                    Task.FromResult <| AuthenticateResult.Success ticket
                with
                    | _ -> Task.FromResult <| AuthenticateResult.Fail "Token serialization failed"
