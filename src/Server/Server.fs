open System.IO
open System
open System.Reflection
open System.Net

open Shared

open Suave
open Suave.Files
open Suave.Filters
open Suave.Operators

open Fable.Remoting.Server
open Fable.Remoting.Suave

module ServerPath =
    let workingDirectory =
        let currentAsm = Assembly.GetExecutingAssembly()
        let codeBaseLoc = currentAsm.CodeBase
        let localPath = Uri(codeBaseLoc).LocalPath
        Directory.GetParent(localPath).FullName

    let resolve segments =
        let paths = Array.concat [| [| workingDirectory |]; Array.ofList segments |]
        Path.GetFullPath(Path.Combine(paths))

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x
let publicPath = ServerPath.resolve [".."; "Client"; "public"]
let port = tryGetEnv "HTTP_PLATFORM_PORT" |> Option.map System.UInt16.Parse |> Option.defaultValue 8085us

let config =
    { defaultConfig with
          homeFolder = Some publicPath
          bindings = [ HttpBinding.create HTTP (IPAddress.Parse "0.0.0.0") port ] }

let counterApi = {
    initialCounter = fun () -> async { return {Value = 42} }
}

let webApi =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue counterApi
    |> Remoting.buildWebPart

let webApp =
    choose [
        webApi
        path "/" >=> browseFileHome "index.html"
        browseHome
        RequestErrors.NOT_FOUND "Not found!"
    ]

startWebServer config webApp