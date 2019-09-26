open System
open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2
open Giraffe
open Saturn
open Shared

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open System.Security.Claims
open System.IdentityModel.Tokens.Jwt

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
    [ Claim(JwtRegisteredClaimNames.Sub, username);
      Claim(JwtRegisteredClaimNames.Jti, Guid.NewGuid().ToString()) ]
    |> Saturn.Auth.generateJWT (secret, algorithm) issuer (DateTime.UtcNow.AddHours(1.0))

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"

let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

let counterApi = {
    initialCounter = fun () -> async { return { Value = 42 } }
}

let securedApi = {
    securedCounter = fun () -> async { return { Value = 69 }}
}

// exmp http://localhost:8080/api/ICounterApi/initialCounter
// exmp http://localhost:8080/api/ISecuredApi/securedCounter
let webApp =

    let userApi =
        Remoting.createApi()
        |> Remoting.withRouteBuilder Route.builder
        |> Remoting.fromValue counterApi
        |> Remoting.buildHttpHandler

    let securedApi =
        let routes =
            Remoting.createApi()
            |> Remoting.withRouteBuilder Route.builder
            |> Remoting.fromValue securedApi
            |> Remoting.buildHttpHandler

        router {
            pipe_through (Saturn.Auth.requireAuthentication Saturn.ChallengeType.JWT)
            forward "" routes
        }

    router {
        forward "" userApi
        forward "" securedApi
    }


let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_router webApp
    use_jwt_authentication secret issuer
    memory_cache
    use_static publicPath
    use_iis
    use_gzip
}

run app
