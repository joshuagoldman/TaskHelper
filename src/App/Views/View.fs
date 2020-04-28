module App.View

open Feliz
open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open App.Types
open Global
open Browser
open Fable.SimpleHttp
open Fable.SimpleHttp
open Feliz.prop
open Fable.Core


//Fable.Core.JsInterop.importAll "../sass/main.sass"

let loginText model dispatch name =
    Html.div
        [
            prop.className "field"
            prop.children
                [
                    Html.div
                        [
                            prop.className "control has-icons-left has-icons-right"
                            prop.children
                                [
                                    Html.input
                                        [
                                            prop.className "input is-success"
                                            prop.type' "text"
                                            prop.placeholder "Text input"
                                        ]
                                    Html.span
                                        [
                                            prop.className "icon is-small is-left"
                                            prop.children
                                                [
                                                    Html.i
                                                        [
                                                            prop.className "fas fa-user"
                                                        ]
                                                ]
                                        ]
                                ] 
                        ]

                ]
        ]

let loginLabel model dispatch name =
    Html.div
        [
            prop.className "field"
            prop.children
                [
                    Html.div
                        [
                            prop.className "label"
                            prop.style
                                [
                                    style.margin 100
                                    style.justifyContent.center
                                    style.display.flex
                                    style.fontSize 30.1
                                ]
                            prop.children
                                [
                                    Fable.React.Helpers.str name
                                ] 
                        ]

                ]
        ]



let logInRoot model dispatch =
    Html.div
        [
            prop.className "rows"
            prop.children
                [
                    Html.div
                        [
                            prop.className "row is"
                            prop.children
                                [
                                    loginLabel model dispatch "Username"
                                    loginText model dispatch "Username"
                                    loginLabel model dispatch "Password"
                                    loginText model dispatch "Password"
                                ]
                        ]

                    Html.div
                        [
                            prop.className "row"
                            prop.children
                                [
                                    
                                ]
                        ]
                ]
        ]


let ff =
    let input = "Joshua GOldman"
    let pattern = "(?<=G).*(?=man)"
    let m = TaskHelperApi.Regex.Match pattern input
    let l = TaskHelperApi.Regex.Matches pattern input
    let n = TaskHelperApi.Regex.IsMatch pattern input

    let result =
        m
        |> function
            | res when res.IsSome ->
                res.Value
            | _ -> "No Match!"

    let res2 =
        l
        |> function
            | res when res.IsSome ->
                res.Value
            | _ -> [|"No Match!"|]

    let res3 =
        n
        |> function
            | res when res.IsSome ->
                res.Value
            | _ -> false
        

    console.log(result)
    console.log(res2)
    console.log(res3)

let root model dispatch =
    match model.User.UserData with
    | Data.Deferred.Resolved (Ok _) ->
        match model.CurrentPage with
        | Global.Page.User _ ->
            User.View.root model.User ( UserMsg >>
                                        dispatch)
        | Login ->
            ff
            Login.View.root model dispatch
    | _ ->
        Login.View.root model dispatch

open Elmish.React
open Elmish.Debug
open Elmish.HMR
open App.State

// App
Program.mkProgram init update root
|> Program.toNavigable (parseHash pageParser) urlUpdate
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReactSynchronous "elmish-app"
|> Program.run
