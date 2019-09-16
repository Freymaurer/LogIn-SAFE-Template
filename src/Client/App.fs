module Client.App

open Elmish
open Elmish.UrlParser
open Elmish.Navigation
open Thoth.Json
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Fulma

type Hero = string

type Route =
    | Home
    | Counter
    | Root
    | Dashboard
    | Detail of int

let toRouteUrl route =
    match route with
    | Route.Root -> "/"
    | Route.Home-> "/home"
    | Route.Counter -> "/counter"
    | Route.Dashboard -> "/dashboard"
    | Route.Detail id -> "/detail/" + string id


type PageModel =
| HomeModel
| CounterModel of Counter.Model

type Model = {
    CurrentRoute : Route option
    CurrentPageModel : PageModel
    Debug : string option
    NumberForDetail : int
}

type Msg =
    | Navigate of Route
    | HomeMsg
    | CounterMsg of Counter.Msg
    | UpdateNumberForDetail of int

module Routing =

    open Elmish.UrlParser
    open Elmish.Navigation

    let private route =
        oneOf [
            map Route.Root (s "")
            map Route.Counter (s "counter")
            map Route.Home (s "home")
            map Route.Dashboard (s "dashboard")
            map (fun detailID -> Route.Detail detailID) (s "detail" </> i32)
        ]

    // Take a window.Location object and return an option of Route

    let parsePath location = UrlParser.parsePath route location

let urlUpdate (route: Route option) (model:Model) =
    match route with
    | None ->
        model, Cmd.ofMsg (Navigate Route.Home)
    | Some Route.Home ->
        { model with
            CurrentPageModel = PageModel.HomeModel
            CurrentRoute = route }, Cmd.none
    | Some Route.Counter ->
        let m, cmd = Counter.init ()
        { model with
            CurrentPageModel = PageModel.CounterModel m
            CurrentRoute = route }, Cmd.map CounterMsg cmd
    | Some (Route.Detail id) ->
            { model with
                CurrentRoute = route
                NumberForDetail = id
                Debug = Some ( sprintf "you just connected to detail number %i" id )
            }, Cmd.none
    | _ ->
        { model with CurrentRoute = route }, Cmd.none

let init _ =
    let model = {
        CurrentRoute = None
        CurrentPageModel = HomeModel
        Debug = None
        NumberForDetail = 0
    }
    let route = Routing.parsePath Browser.Dom.document.location
    urlUpdate route model

let update msg (currentModel:Model) =
    match msg, currentModel.CurrentPageModel with
    | Navigate route , _  ->
        currentModel, Navigation.newUrl (toRouteUrl route)
    | HomeMsg, HomeModel ->
        let nextModel = {
            currentModel with
                CurrentPageModel = HomeModel
                CurrentRoute = Some Home
            }
        nextModel, Cmd.none
    | CounterMsg msg, CounterModel m ->
        let m, cmd =
            Counter.update msg m
        let nextModel = {
            currentModel with
                CurrentPageModel = CounterModel m
                CurrentRoute = Some Counter
            }
        nextModel, Cmd.map CounterMsg cmd
    | UpdateNumberForDetail (inputVal), _ ->
        let nextModel = {
            currentModel with
                NumberForDetail = inputVal
        }
        nextModel,Cmd.none
    | _ -> currentModel,Cmd.none


let heroHeadNavbar dispatch (model:Model)=
    Hero.head [] [
        Tabs.tabs [ Tabs.IsBoxed; Tabs.IsCentered ] [
        a [Href "#"; OnClick (fun ev -> ev.preventDefault(); Msg.Navigate Route.Root |> dispatch)] [str "Root"]
        a [Href "#"; OnClick (fun ev -> ev.preventDefault(); Msg.Navigate Route.Dashboard |> dispatch)] [str "Dashboard"]
        a [Href "#"; OnClick (fun ev -> ev.preventDefault(); Msg.Navigate Route.Home |> dispatch)] [str "Home"]
        a [Href "#"; OnClick (fun ev -> ev.preventDefault(); Msg.Navigate Route.Counter |> dispatch)] [str "Counter"]
        a [Href "#"; OnClick (fun ev -> ev.preventDefault(); Msg.Navigate (Route.Detail model.NumberForDetail) |> dispatch)] [str (sprintf "Detail %i" model.NumberForDetail)]
        ]
    ]

let safeComponents =
    let components =
        span [ ]
           [ a [ Href "https://github.com/SAFE-Stack/SAFE-template" ]
               [ str "SAFE  "
                 str Version.template ]
             str ", "
             a [ Href "http://suave.io" ] [ str "Suave" ]
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

let view (model:Model) (dispatch: Msg -> unit) =
    Hero.hero [ Hero.IsHalfHeight ] [
        (heroHeadNavbar dispatch model)
        //str model.Debug.Value
        Columns.columns [ Columns.IsCentered ][
            Column.column [ Column.Width (Screen.All,Column.Is2) ] [
              Input.number [ Input.Placeholder "Detail Number..."
                             Input.OnChange (fun e -> let x = !!e.target?value
                                                      dispatch (UpdateNumberForDetail x)
                                            )
                           ]
            ]
        ]
        Columns.columns [ Columns.IsCentered ] [
            Column.column [ Column.Width (Screen.All,Column.IsHalf) ] [
                Box.box' [ ] [
                    match model.CurrentPageModel, model.CurrentRoute with
                    | _, Some (Route.Detail id) ->
                            yield str (sprintf "you just matched the Detail Route with id: %i" id)
                    | HomeModel, Some (Dashboard) ->
                            yield str "you just matched the Dashboard Route"
                    | HomeModel, _ -> 
                            yield Home.view ()
                    | CounterModel m ,Some Route.Counter->
                            yield Counter.view { Model = m; Dispatch = (CounterMsg >> dispatch) }
                    | _ ->
                            yield str "this does not exist yet"
                ]  
            ]
        ]
        Hero.foot [] [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [ safeComponents ]
        ] 
    ]


open Elmish.React
open Elmish.Debug
open Elmish.Navigation


#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withDebugger
#endif
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.toNavigable Routing.parsePath urlUpdate
|> Program.withReactBatched "elmish-app"
|> Program.run