module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fulma
open Thoth.Json

open Shared

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type Model = {
    Counter: Counter option
    ErrorMsg : string option
    }

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
    | Increment
    | Decrement
    | InitialCountLoaded of Counter
    | GetSecuredCounterRequest
    | GetSecuredCounterResponse of Result<Counter, exn>

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

    let securedApi : ISecuredApi =
      Remoting.createApi()
      |> Remoting.withRouteBuilder normalizeRoutes
      |> Remoting.buildProxy<ISecuredApi>



let initialCounter = Server.userApi.initialCounter

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let initialModel = {
        Counter = None
        ErrorMsg = None
    }
    let loadCountCmd =
        Cmd.OfAsync.perform initialCounter () InitialCountLoaded
    initialModel, loadCountCmd

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match currentModel.Counter, msg with
    | Some counter, Increment ->
        let nextModel = { currentModel with Counter = Some { Value = counter.Value + 1 } }
        nextModel, Cmd.none
    | Some counter, Decrement ->
        let nextModel = { currentModel with Counter = Some { Value = counter.Value - 1 } }
        nextModel, Cmd.none
    | _, InitialCountLoaded initialCount->
        let nextModel = { currentModel with Counter = Some initialCount }
        nextModel, Cmd.none
    | Some _ , GetSecuredCounterRequest ->
        let requestCmd =
            Cmd.OfAsync.either
                Server.securedApi.securedCounter
                ()
                (Ok >> GetSecuredCounterResponse)
                (Error >> GetSecuredCounterResponse)
        let nextModel = {
            currentModel with Counter = None
            }
        nextModel, requestCmd
    | _ , GetSecuredCounterResponse (Error e) ->
        let nextModel = {
            currentModel with
                ErrorMsg = Some e.Message
        }
        let loadCountCmd =
            Cmd.OfAsync.perform initialCounter () InitialCountLoaded
        nextModel, loadCountCmd
    | _ , GetSecuredCounterResponse (Ok value) ->
        let nextModel = {
            currentModel with
                Counter = Some value
        }
        nextModel, Cmd.none
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
    | { Counter = Some counter } -> string counter.Value
    | { Counter = None   } -> "Loading..."

let button txt onClick =
    Button.button
        [ Button.IsFullWidth
          Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]

let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ ]
                    [ str "SAFE Template" ] ] ]

          Container.container []
              [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ Heading.h3 [] [ str ("Press buttons to manipulate counter: " + show model) ] ]
                Columns.columns []
                    [ Column.column [] [ button "-" (fun _ -> dispatch Decrement) ]
                      Column.column [] [ button "+" (fun _ -> dispatch Increment) ]
                      Column.column [] [ button "secret" (fun _ -> dispatch GetSecuredCounterRequest) ] ] ]

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
