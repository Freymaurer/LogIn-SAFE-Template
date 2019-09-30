open System
open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2
open Saturn
open Shared

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Giraffe
open System.Security.Claims
open System.IdentityModel.Tokens.Jwt
open FSharp.Control.Tasks.V2.ContextInsensitive
open Microsoft.AspNetCore.Http


module MyJWT =

    let private createPassPhrase() =
        let crypto = System.Security.Cryptography.RandomNumberGenerator.Create()
        let randomNumber = Array.init 32 byte
        crypto.GetBytes(randomNumber)
        randomNumber

    let secret =
        let fi = FileInfo("./temp/token.txt")
        if not fi.Exists then
            let passPhrase = createPassPhrase()
            if not fi.Directory.Exists then
                fi.Directory.Create()
            File.WriteAllBytes(fi.FullName,passPhrase)
        File.ReadAllBytes(fi.FullName)
        |> System.Text.Encoding.UTF8.GetString

    let issuer = "TestLogIn.io"

    let private algorithm = Microsoft.IdentityModel.Tokens.SecurityAlgorithms.HmacSha256

    let generateToken username =
        let claims = [|
            Claim(JwtRegisteredClaimNames.Sub, username);
            Claim(JwtRegisteredClaimNames.Jti, Guid.NewGuid().ToString()) |]
        claims
        |> Auth.generateJWT (secret, algorithm) issuer (DateTime.UtcNow.AddYears(1))
        |> fun x -> { Token = x }


let jwtPayload (token:string) =
    let token = System.IdentityModel.Tokens.Jwt.JwtSecurityTokenHandler().ReadJwtToken(token)
    let claims =
        token
        |> fun x -> x.Claims
    let test =
        claims |> (Array.ofSeq >> Array.map (fun x -> x.Value) )
    let test2 =
        token.Payload.Sub
    //claims |> (Array.ofSeq >> Array.head >> fun x -> x.Value)
    //String.concat ";" test2
    test2

let verifyUser (user:User) =
    //here could be any user authentification
    if user.Username = "testUser" && user.Password = "testPw"
    then Some (MyJWT.generateToken user.Username)
    else None

let validateToken (tokenStr:string) =
    let key:string = MyJWT.secret
    let tokenHandler =
        new System.IdentityModel.Tokens.Jwt.JwtSecurityTokenHandler()
    let securityToken =
        tokenHandler.ReadToken(tokenStr)
    let hmac =
        new Security.Cryptography.HMACSHA256(System.Text.Encoding.ASCII.GetBytes(key))
    let symKey =
        new Microsoft.IdentityModel.Tokens.SymmetricSecurityKey(hmac.Key)
    let symKey2 =
        key
        |> fun x -> x.ToCharArray()
        |> Array.map (fun c -> Byte.Parse(string c) )
        |> fun x -> new Microsoft.IdentityModel.Tokens.SymmetricSecurityKey(x)
    let validationParams =
        new Microsoft.IdentityModel.Tokens.TokenValidationParameters()
    validationParams.IssuerSigningKey <- symKey2
    tokenHandler.ValidateToken(tokenStr,validationParams)
    |> fun x -> fst x
    |> fun x -> x.ToString()
    

let getUserFromToken (tokenStr:string option) =
    if tokenStr.IsSome
    then 
        let token = System.IdentityModel.Tokens.Jwt.JwtSecurityTokenHandler().ReadJwtToken(tokenStr.Value)
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
    getToken = fun (user) -> async { return (verifyUser user) }
    getTest = fun (token) -> async { return validateToken token(*(getUserFromToken (Some token)).Value*)}
}

let securedApi = {
    securedCounter = fun () -> async { return { Value = 69 } }
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
    use_jwt_authentication MyJWT.secret MyJWT.issuer
    use_router webApp
    url ("http://0.0.0.0:" + port.ToString() + "/")
    memory_cache
    use_static publicPath
    use_iis
    use_gzip
}

run app
