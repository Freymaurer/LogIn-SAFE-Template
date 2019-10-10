open System
open System.IO
open Saturn
open Shared

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Giraffe
open System.Security.Claims
open System.IdentityModel.Tokens.Jwt

type UserInfo =
    { Username : string
      Claims : string [] }



module MyJWT =


    // Do not have this in the repo, save this in an extra file, which will not be pushed anywhere and is only saved locally!
    let passPhrase = @"d38mQ!91*D9PW$dwHQsB%f3Q^Dsz@M7VDuqZvDWU!%U5nPl7BV![#x{/7.N?X^b]GfpYSrL_:~NB[MA~WVSmnX&9V."

    let issuer = "TestLogIn.io"

    let private algorithm = Microsoft.IdentityModel.Tokens.SecurityAlgorithms.HmacSha256

    let generateToken username =
        let claims = [|
            Claim(JwtRegisteredClaimNames.Sub, username);
            (*Claim(JwtRegisteredClaimNames.Jti, Guid.NewGuid().ToString()) *)|]
        claims
        |> Auth.generateJWT (passPhrase, algorithm) issuer (DateTime.UtcNow.AddYears(1))
        |> fun x -> { Token = x }

let verifyUser (user:User) =
    //here could be any user authentification
    if user.Username = "test" && user.Password = "test"
    then LogInResults.Success (MyJWT.generateToken user.Username)
    else LogInResults.InvalidPasswordUser


///// Can use this to readk Jwt token and extract information in payload!
//let jwtPayload (token:string) =
//    let token = System.IdentityModel.Tokens.Jwt.JwtSecurityTokenHandler().ReadJwtToken(token)
//    let claims =
//        token
//        |> fun x -> x.Claims
//    let test =
//        claims |> (Array.ofSeq >> Array.map (fun x -> x.Value) )
//    let test2 =
//        token.Payload.Sub
//    //claims |> (Array.ofSeq >> Array.head >> fun x -> x.Value)
//    //String.concat ";" test2
//    test2

// https://docs.microsoft.com/en-us/dotnet/api/system.identitymodel.tokens.jwt.jwtsecuritytokenhandler.validatetoken?view=azure-dotnet
// https://stackoverflow.com/questions/42964432/validating-jwt-with-microsofts-identitymodel
let validateToken (tokenStr:string) =

    let key:string = MyJWT.passPhrase

    let tokenHandler =
        new System.IdentityModel.Tokens.Jwt.JwtSecurityTokenHandler()

    let hmac =
        new Security.Cryptography.HMACSHA256(System.Text.Encoding.ASCII.GetBytes(key))

    let symKey =
        new Microsoft.IdentityModel.Tokens.SymmetricSecurityKey(hmac.Key)

    let validationParams =
        new Microsoft.IdentityModel.Tokens.TokenValidationParameters()

    validationParams.IssuerSigningKey <- symKey
    validationParams.ValidIssuer <- MyJWT.issuer
    validationParams.ValidAudience <- MyJWT.issuer

    let (user,_) =
        printfn "try validating the token: %s" tokenStr
        tokenHandler.ValidateToken(tokenStr, validationParams)

    user.Identity.IsAuthenticated

let getUserFromToken (tokenStr:string) =
    if validateToken tokenStr = true
    then 
        let token = System.IdentityModel.Tokens.Jwt.JwtSecurityTokenHandler().ReadJwtToken(tokenStr)
        let user =
            token.Payload.Sub
        Some user
    else None

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"

let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

let counterApi = {
    initialCounter = fun () -> async {return { Value = 42 }}
    initialLoad = fun (token) -> async { return ((getUserFromToken token),{ Value = 42 }) }
    logIn = fun (user) -> async { return (verifyUser user) }
    validateToken = fun (token) -> async { return validateToken token}
}

let nice() =
    printfn "nice"
    { Value = 69 }

let securedApi = {
    securedCounter = fun () -> async { return (nice()) }
}


// exmp http://localhost:8080/api/ICounterApi/initialCounter
// exmp http://localhost:8080/api/ISecuredApi/securedCounter
let webApp =

    let userApi =
        Remoting.createApi()
        |> Remoting.withRouteBuilder Route.builder
        |> Remoting.fromValue counterApi
        |> Remoting.withDiagnosticsLogger (printfn "%s")
        |> Remoting.buildHttpHandler

    let securedApi =  

        let routes =
            Remoting.createApi()
            |> Remoting.withRouteBuilder Route.builder
            |> Remoting.fromValue securedApi
            |> Remoting.withDiagnosticsLogger (printfn "%s")
            |> Remoting.buildHttpHandler

        router {
            pipe_through (Saturn.Auth.requireAuthentication Saturn.ChallengeType.JWT)
            forward "" routes
        }

    router {
        not_found_handler (setStatusCode 404 >=> text "Not Found")
        forward "" userApi
        forward "" securedApi
    }


let app = application {
    use_jwt_authentication MyJWT.passPhrase MyJWT.issuer
    use_router webApp
    url ("http://0.0.0.0:" + port.ToString() + "/")
    memory_cache
    use_static publicPath
    use_iis
    use_gzip
}

run app
