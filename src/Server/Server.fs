open System
open System.IO
open Saturn
open Shared

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Giraffe

open Microsoft.AspNetCore.Identity
open Microsoft.Extensions.DependencyInjection
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Identity.EntityFrameworkCore
open Microsoft.EntityFrameworkCore
open FSharp.Control.Tasks
open Microsoft.AspNetCore.Builder

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"

let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

module DotnetModule =

    open System.Text

    let showErrors (errors : IdentityError seq) =
        errors
        |> Seq.fold (fun acc err ->
            sprintf "Code: %s, Description: %s" err.Code err.Description
            |> acc.AppendLine : StringBuilder) (StringBuilder(""))
        |> (fun x -> x.ToString())

    let dotnetLogin (user:LoginModel) (context: HttpContext) =
        task {
            let signInManager = context.GetService<SignInManager<IdentityUser>>()
            let! result = signInManager.PasswordSignInAsync(user.Username, user.Password, true, false)
            match result.Succeeded with
            | true ->
               return LoginSuccess (result.ToString())
            | false -> return LoginFail (result.ToString())
        } |> fun x -> x.Result

    let dotnetGetUser (context: HttpContext) =
        task {
            let userManager = context.GetService<UserManager<IdentityUser>>()
            let! user = userManager.GetUserAsync context.User
            return { Username = user.UserName; Email = user.Email }
        } |> fun x -> x.Result

    let dotnetUserLogOut (context: HttpContext) =
        task {
            let signInManager = context.GetService<SignInManager<IdentityUser>>()
            do! signInManager.SignOutAsync()
            return LogoutSuccess "Log Out Success"
        } |> fun x -> x.Result

    let dotnetRegistration (registerModel:RegisterModel) (context: HttpContext) =

        task {
            let  user        = IdentityUser(UserName = registerModel.Username, Email = registerModel.Email)
            let  userManager = context.GetService<UserManager<IdentityUser>>()
            let! result      = userManager.CreateAsync(user, registerModel.Password)
            match result.Succeeded with
            | false -> return (RegisterFail (showErrors result.Errors))
            | true  ->
                let signInManager = context.GetService<SignInManager<IdentityUser>>()
                do! signInManager.SignInAsync(user, true)
                return RegisterSuccess "Registration Successful"
        } |> fun x -> x.Result

let dotnetApi (context: HttpContext) = {
    dotnetLogin = fun (loginModel) -> async { return (DotnetModule.dotnetLogin loginModel context) }
    dotnetRegister = fun (registerModel) -> async { return (DotnetModule.dotnetRegistration registerModel context) }
}

let nice() =
    { Value = 69 }

let dotnetSecureApi (context: HttpContext) = {
    dotnetGetUser = fun () -> async { return (DotnetModule.dotnetGetUser context)}
    dotnetUserLogOut = fun () -> async { return (DotnetModule.dotnetUserLogOut context) }
    getUserCounter = fun () -> async { return nice() }
}

let counterApi = {
    initialCounter = fun () -> async {return { Value = 42 }}
}

module MyGiraffe =

    open Giraffe.GiraffeViewEngine
    open System.Text
    open Microsoft.Extensions.Logging
    open Microsoft.AspNetCore.Cors.Infrastructure

    let masterPage (pageTitle : string) (content : XmlNode list) =
        html [] [
            head [] [
                title [] [ str pageTitle ]
                style [] [ rawText "label { display: inline-block; width: 80px; }" ]
            ]
            body [] [
                h1 [] [ str pageTitle ]
                main [] content
             ]
        ]

    let indexPage =
        [
            p [] [
                a [ _href "/mygtest/register" ] [ str "Register" ]
            ]
            p [] [
                a [ _href "/mygtest/user" ] [ str "User page" ]
            ]
        ] |> masterPage "Home"

    let registerPage =
        [
            form [ _action "/mygtest/register"; _method "POST" ] [
                div [] [
                    label [] [ str "Email:" ]
                    input [ _name "Email"; _type "text" ]
                ]
                div [] [
                    label [] [ str "User name:" ]
                    input [ _name "UserName"; _type "text" ]
                ]
                div [] [
                    label [] [ str "Password:" ]
                    input [ _name "Password"; _type "password" ]
                ]
                input [ _type "submit" ]
            ]
        ] |> masterPage "Register"

    let loginPage (loginFailed : bool) =
        [
            if loginFailed then yield p [ _style "color: Red;" ] [ str "Login failed." ]

            yield form [ _action "/mygtest/login"; _method "POST" ] [
                div [] [
                    label [] [ str "User name:" ]
                    input [ _name "UserName"; _type "text" ]
                ]
                div [] [
                    label [] [ str "Password:" ]
                    input [ _name "Password"; _type "password" ]
                ]
                input [ _type "submit" ]
            ]
            yield p [] [
                str "Don't have an account yet?"
                a [ _href "/mygtest/register" ] [ str "Go to registration" ]
            ]
        ] |> masterPage "Login"

    let userPage (user : IdentityUser) =
        [
            p [] [
                sprintf "User name: %s, Email: %s" user.UserName user.Email
                |> str
            ]
        ] |> masterPage "User details"

    // ---------------------------------
    // Web app
    // ---------------------------------

    [<CLIMutable>]
    type RegisterModel =
        {
            UserName : string
            Email    : string
            Password : string
        }

    [<CLIMutable>]
    type LoginModel =
        {
            UserName : string
            Password : string
        }

    let showErrors (errors : IdentityError seq) =
        errors
        |> Seq.fold (fun acc err ->
            sprintf "Code: %s, Description: %s" err.Code err.Description
            |> acc.AppendLine : StringBuilder) (StringBuilder(""))
        |> (fun x -> x.ToString())
        |> text

    let registerHandler : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let! model       = ctx.BindFormAsync<RegisterModel>()
                let  user        = IdentityUser(UserName = model.UserName, Email = model.Email)
                let  userManager = ctx.GetService<UserManager<IdentityUser>>()
                let! result      = userManager.CreateAsync(user, model.Password)

                match result.Succeeded with
                | false -> return! showErrors result.Errors next ctx
                | true  ->
                    let signInManager = ctx.GetService<SignInManager<IdentityUser>>()
                    do! signInManager.SignInAsync(user, true)
                    return! redirectTo false "/mygtest/user" next ctx
            }

    let loginHandler : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let! model = ctx.BindFormAsync<LoginModel>()
                let signInManager = ctx.GetService<SignInManager<IdentityUser>>()
                let! result = signInManager.PasswordSignInAsync(model.UserName, model.Password, true, false)
                match result.Succeeded with
                | true  -> return! redirectTo false "/mygtest/user" next ctx
                | false -> return! htmlView (loginPage true) next ctx
            }

    let userHandler : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let userManager = ctx.GetService<UserManager<IdentityUser>>()
                let! user = userManager.GetUserAsync ctx.User
                return! (user |> userPage |> htmlView) next ctx
            }

    let mustBeLoggedIn : HttpHandler =
        requiresAuthentication (redirectTo false "/mygtest/login")

    let logoutHandler : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let signInManager = ctx.GetService<SignInManager<IdentityUser>>()
                do! signInManager.SignOutAsync()
                return! (redirectTo false "/") next ctx
            }

    // ---------------------------------
    // Error handler
    // ---------------------------------

    let errorHandler (ex : Exception) (logger : ILogger) =
        logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
        clearResponse >=> setStatusCode 500 >=> text ex.Message

    // ---------------------------------
    // Main
    // ---------------------------------

    let configureCors (builder : CorsPolicyBuilder) =
        builder.WithOrigins("http://localhost:8080").AllowAnyMethod().AllowAnyHeader() |> ignore

    let configureApp (app : IApplicationBuilder) =
        app.UseCors(configureCors)
           .UseGiraffeErrorHandler(errorHandler)
           .UseAuthentication()


    let configureServices (services : IServiceCollection) =
        // Configure InMemory Db for sample application
        services.AddDbContext<IdentityDbContext<IdentityUser>>(
            fun options ->
                options.UseInMemoryDatabase("NameOfDatabase") |> ignore
            ) |> ignore

        // Register Identity Dependencies
        services.AddIdentity<IdentityUser, IdentityRole>(
            fun options ->
                // Password settings
                options.Password.RequireDigit   <- true
                options.Password.RequiredLength <- 8
                options.Password.RequireNonAlphanumeric <- false
                options.Password.RequireUppercase <- true
                options.Password.RequireLowercase <- false

                // Lockout settings
                options.Lockout.DefaultLockoutTimeSpan  <- TimeSpan.FromMinutes 30.0
                options.Lockout.MaxFailedAccessAttempts <- 10

                // User settings
                options.User.RequireUniqueEmail <- true
            )
            .AddEntityFrameworkStores<IdentityDbContext<IdentityUser>>()
            .AddDefaultTokenProviders()
            |> ignore

        // Configure app cookie
        services.ConfigureApplicationCookie(
            fun options ->
                options.ExpireTimeSpan <- TimeSpan.FromDays 150.0
                options.LoginPath      <- PathString "/mygtest/login"
                options.LogoutPath     <- PathString "/mygtest/logout"
            ) |> ignore

        // Enable CORS
        services.AddCors() |> ignore

        // Configure Giraffe dependencies
        services.AddGiraffe() |> ignore
        services

    let configureLogging (builder : ILoggingBuilder) =
        let filter (l : LogLevel) = l.Equals LogLevel.Error
        builder.AddFilter(filter).AddConsole().AddDebug() |> ignore

// exmp http://localhost:8080/api/ICounterApi/initialCounter
// exmp http://localhost:8080/api/ISecuredApi/securedCounter
let webApp =

    let userApi =
        Remoting.createApi()
        |> Remoting.withRouteBuilder Route.builder
        |> Remoting.fromValue counterApi
        |> Remoting.withDiagnosticsLogger (printfn "%s")
        |> Remoting.buildHttpHandler

    let dotnetServiceApi =
        Remoting.createApi()
        |> Remoting.withRouteBuilder Route.builder
        |> Remoting.fromContext dotnetApi
        |> Remoting.withDiagnosticsLogger (printfn "%s")
        |> Remoting.buildHttpHandler

    let dotnetSecureApi =
        Remoting.createApi()
        |> Remoting.withRouteBuilder Route.builder
        |> Remoting.fromContext dotnetSecureApi
        |> Remoting.withDiagnosticsLogger (printfn "%s")
        |> Remoting.buildHttpHandler

    let myPaths =
        router {
            get "/home" (htmlView MyGiraffe.indexPage)
            get "/register" (htmlView MyGiraffe.registerPage)
            get "/login" (htmlView (MyGiraffe.loginPage false))
            get "/logout" (MyGiraffe.mustBeLoggedIn >=> MyGiraffe.logoutHandler)
            get "/user" (MyGiraffe.mustBeLoggedIn >=> MyGiraffe.userHandler)
            post "/register" MyGiraffe.registerHandler
            post "/login" MyGiraffe.loginHandler
        }

    let mustBeLoggedIn : HttpHandler =
        requiresAuthentication (redirectTo false "/")

    router {
        not_found_handler (setStatusCode 404 >=> text "Not Found")
        forward "/mygtest" myPaths
        forward "" dotnetServiceApi
        forward "" userApi
        forward "" (mustBeLoggedIn >=> dotnetSecureApi)
    }

let app = application {
    use_cookies_authentication ""//"lambdafactory.io"
    use_router webApp
    url ("http://0.0.0.0:" + port.ToString() + "/")
    service_config MyGiraffe.configureServices
    memory_cache
    use_static publicPath
    use_iis
    use_gzip
    }

run app