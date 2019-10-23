module Client

open System

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fulma
open Thoth.Json
open Fable.Core
open Shared

let [<Literal>] ENTER_KEY = 13.

let onEnter msg dispatch =
    OnKeyDown (fun ev ->
        if ev.keyCode = ENTER_KEY then
            dispatch msg)

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type Model = {
    Counter: Counter option
    ErrorMsg : string option
    User : User option
    Loading : bool
    Authenticated : bool
    }

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
    | Increment
    | Decrement
    | InitialCountLoaded of Counter
    | UpdateUsername of string
    | UpdateUserPw of string
    | Debug of string
    | GetTestRequest of string
    | GetTestResponse of Result<string, exn>
    | DotnetLogInRequest of User
    | DotnetLogInResponse of Result<DotnetLogInResults,exn>
    | DotnetGetUserRequest
    | DotnetGetUserResponse of Result<User,exn>
    | DotnetUserLogOutRequest
    | DotnetUserLogOutResponse of Result<DotnetLogOutResults,exn>
    | GetUserCounterRequest
    | GetUserCounterResponse of Result<Counter,exn>

module ServerPath =
    open System
    open Fable.Core

    /// when publishing to IIS, your application most likely runs inside a virtual path (i.e. localhost/SafeApp)
    /// every request made to the server will have to account for this virtual path
    /// so we get the virtual path from the location
    /// `virtualPath` of `http://localhost/SafeApp` -> `/SafeApp/`
    [<Emit("window.location.pathname")>]
    let virtualPath : string = jsNative

    /// takes path segments and combines them into a valid path
    let combine (paths: string list) =
        paths
        |> List.map (fun path -> List.ofArray (path.Split('/')))
        |> List.concat
        |> List.filter (fun segment -> not (segment.Contains(".")))
        |> List.filter (String.IsNullOrWhiteSpace >> not)
        |> String.concat "/"
        |> sprintf "/%s"

    /// Normalized the path taking into account the virtual path of the server
    let normalize (path: string) = combine [virtualPath; path]

module Server =

    open Shared
    open Fable.Remoting.Client

    // normalize routes so that they work with IIS virtual path in production
    let normalizeRoutes typeName methodName =
        Route.builder typeName methodName
        |> ServerPath.normalize

    /// A proxy you can use to talk to server directly
    let userApi : ICounterApi =
        Remoting.createApi()
        |> Remoting.withRouteBuilder normalizeRoutes
        |> Remoting.buildProxy<ICounterApi>

    let dotnetApi : IDotnetApi =
        Remoting.createApi()
        |> Remoting.withRouteBuilder normalizeRoutes
        |> Remoting.buildProxy<IDotnetApi>

    let dotnetSecureApi : IDotnetSecureApi =
        Remoting.createApi()
        |> Remoting.withRouteBuilder normalizeRoutes
        |> Remoting.buildProxy<IDotnetSecureApi>

let myDecode64 (str64:string) =
    let l = str64.Length
    let padNum = l%4
    let padding = if padNum = 0 then "" else Array.init (4-padNum) (fun _ -> "=") |> String.concat ""
    let newStr = str64 + padding
    let toByteArr = System.Convert.FromBase64String(newStr)
    System.Text.Encoding.UTF8.GetString (toByteArr)

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let initialModel = {
        Counter = None
        ErrorMsg = None
        User = None
        Loading = true
        Authenticated = false
    }
    let loadCountCmd =
        Cmd.OfAsync.perform
                Server.userApi.initialCounter
                ()
                InitialCountLoaded
    initialModel, loadCountCmd

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match currentModel.Counter, msg with
        /// functions to manage counter
    | Some counter, Increment ->
        let nextModel = { currentModel with Counter = Some { Value = counter.Value + 1 } }
        nextModel, Cmd.none
    | Some counter, Decrement ->
        let nextModel = { currentModel with Counter = Some { Value = counter.Value - 1 } }
        nextModel, Cmd.none
    | _, InitialCountLoaded initialCount ->
        let nextModel = { currentModel with Counter = Some initialCount; Loading = false }
        nextModel, Cmd.none
        /// functions to manage input fields for user log in
    | _ , UpdateUsername (name:string) ->
        let nextModel = {
            currentModel with
                User = if currentModel.User.IsSome then Some {currentModel.User.Value with Username = name} else Some { Username = name; Password = ""}
        }
        nextModel, Cmd.none
    | _ , UpdateUserPw (pw:string) ->
        let nextModel = {
            currentModel with
                User = if currentModel.User.IsSome then Some {currentModel.User.Value with Password = pw} else Some { Password = pw; Username = ""}
        }
        nextModel, Cmd.none
        /// functions to log in user via asp.net 
    | _, DotnetLogInRequest (user) ->
        let cmdLogIn =
            Cmd.OfAsync.either
                Server.dotnetApi.dotnetLogIn
                user
                (Result.Ok >> DotnetLogInResponse)
                (Result.Error >> DotnetLogInResponse)
        let nextModel = {
            currentModel with
                Loading = true
            }
        nextModel,cmdLogIn
    | _ , DotnetLogInResponse (Result.Error e) ->
        let nextModel = {
            currentModel with
                ErrorMsg = Some e.Message
                Loading = false
        }
        nextModel,Cmd.none
    | _ , DotnetLogInResponse (Result.Ok value) ->
        let x =
            match value with
            | DotnetLogInResults.Success _ -> "Log In Succeded"
            | DotnetLogInResults.Failed _ -> "Log In Failed"
        let nextModel = {
            currentModel with
                ErrorMsg = Some x
        }
        nextModel, Cmd.ofMsg DotnetGetUserRequest
        /// functions to access already logged in user information
    | _, DotnetGetUserRequest ->
        let cmd =
            Cmd.OfAsync.either
                Server.dotnetSecureApi.dotnetGetUser
                ()
                (Result.Ok >> DotnetGetUserResponse)
                (Error >> DotnetGetUserResponse)
        currentModel, cmd
    | _, DotnetGetUserResponse (Ok value) ->
        let nextModel = {
            currentModel with
                Authenticated = true
                User = Some value
                Loading = false
        }
        nextModel,Cmd.none
    | _, DotnetGetUserResponse (Error e) ->
        let nextModel = {
            currentModel with
                Loading = false
                ErrorMsg = Some e.Message
        }
        nextModel,Cmd.none
        /// functions to access user-only counter
    | _, GetUserCounterRequest ->
        let nextModel = {
            currentModel with
                Loading = true
        }
        let cmd =
            Cmd.OfAsync.either
                Server.dotnetSecureApi.getUserCounter
                ()
                (Ok >> GetUserCounterResponse)
                (Error >> GetUserCounterResponse)
        nextModel, cmd
    | _, GetUserCounterResponse (Ok value)->
        let nextModel = {
            currentModel with
                Loading = false
                Counter = Some value
        }
        nextModel, Cmd.none
    | _, GetUserCounterResponse (Error e)->
        let nextModel = {
            currentModel with
                Loading = false
                ErrorMsg = Some e.Message
        }
        nextModel, Cmd.none
        /// functions to handle user log out
    | _, DotnetUserLogOutRequest ->
        let cmd =
            Cmd.OfAsync.either
                Server.dotnetSecureApi.dotnetUserLogOut
                ()
                (Ok >> DotnetUserLogOutResponse)
                (Error >> DotnetUserLogOutResponse)
        let nextModel = {
            currentModel with
                Loading = true
        }
        nextModel, cmd
    | _, DotnetUserLogOutResponse (Ok value) ->
        init()
    | _, DotnetUserLogOutResponse (Error e) ->
        let nextModel = {
            currentModel with
                ErrorMsg = Some e.Message
        }
        nextModel, Cmd.none
    | _, Debug (message) ->
        { currentModel with ErrorMsg = Some message}, Cmd.none
    | _ -> currentModel, Cmd.none

let safeComponents =
    let components =
        span [ ]
           [ a [ Href "https://github.com/SAFE-Stack/SAFE-template" ]
               [ str "SAFE  "
                 str Version.template ]
             str ", "
             a [ Href "https://saturnframework.github.io" ] [ str "Saturn" ]
             str ", "
             a [ Href "http://fable.io" ] [ str "Fable" ]
             str ", "
             a [ Href "https://elmish.github.io" ] [ str "Elmish" ]
             str ", "
             a [ Href "https://fulma.github.io/Fulma" ] [ str "Fulma" ]
             str ", "
             a [ Href "https://zaid-ajaj.github.io/Fable.Remoting/" ] [ str "Fable.Remoting" ]

           ]

    span [ ]
        [ str "Version "
          strong [ ] [ str Version.app ]
          str " powered by: "
          components ]

let debug (m:Model) =
    match m with
    | { ErrorMsg = Some value } -> string m.ErrorMsg
    | { ErrorMsg = None } -> ""

let show model =
    match model with
    | { Counter = Some counter;Loading = false } -> string counter.Value
    | _ -> "Loading..."

let button txt onClick =
    Button.button
        [ Button.IsFullWidth
          Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]

let loginNavbar (model : Model) (dispatch : Msg -> unit)=
    [ Navbar.Item.div
        [ ]
        [ Heading.h2 [ ]
            [ str "SAFE Template - Login" ] ]
      Navbar.End.a
        [ ]
        [
            Navbar.Item.div
              [ ]
              [ Input.text
                  [ Input.OnChange (
                      fun e ->
                          //let newUser = { model.User with Username = e.Value}
                          dispatch (UpdateUsername e.Value)
                      )
                    Input.Placeholder "Username"
                    Input.Props [ Id "UserName" ]
                      ]
              ]
            Navbar.Item.div
              [ ]
              [ Input.password
                  [ Input.OnChange (
                      fun e ->
                          //let newUser = { model.User with Password = e.Value}
                          dispatch (UpdateUserPw e.Value)
                      )
                    Input.Placeholder "Password"
                    Input.Props [ Id "UserPw" ]
                      ]
              ]
            Navbar.Item.div
              [ ]
              [ Button.a
                  [ Button.OnClick (fun _ -> dispatch (DotnetLogInRequest model.User.Value)) ]
                  [ str "Login" ]
              ]
        ]
    ]

let loggedInNavbar (model : Model) (dispatch : Msg -> unit) =
    [
        Navbar.Item.div
            [ ]
            [ Heading.h2 [ ]
                [ str "SAFE Template - Login" ]
            ]
        Navbar.End.a [ ] [
            Navbar.Item.div [
                Navbar.Item.IsHoverable;
                Navbar.Item.HasDropdown;
            ] [
                Navbar.Link.a [ Navbar.Link.IsArrowless ] [
                    Text.span
                        [ Modifiers [ Modifier.TextWeight TextWeight.SemiBold; Modifier.TextColor Color.IsWhiteBis ] ]
                        [ str (if model.User.IsSome then model.User.Value.Username else "No User Information")]
                ]
                Navbar.Dropdown.div [ Navbar.Dropdown.IsRight ] [
                    Navbar.divider [ ] [ ]
                    Navbar.Item.a
                        [ Navbar.Item.Props [OnClick ( fun _ -> dispatch DotnetUserLogOutRequest)] ]
                        [ str "Logout" ]
                ]
            ]
            Navbar.Item.div [] [br []]
        ]
    ]

let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            (if model.Authenticated = true then (loggedInNavbar model dispatch) else (loginNavbar model dispatch ))
          br []
          Container.container []
              [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ Heading.h3 [] [ str ("Press buttons to manipulate counter: " + show model) ] ]
                Columns.columns []
                    [ Column.column [] [ button "-" (fun _ -> dispatch Decrement) ]
                      Column.column [] [ button "+" (fun _ -> dispatch Increment) ]
                      Column.column [] [ button "secret" (fun _ -> dispatch GetUserCounterRequest) ] ] ]
          Footer.footer [ ]
                [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ safeComponents
                      br []
                      str (debug model) ] ] ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run