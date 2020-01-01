module Login.View

open Feliz
open State
open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Types
open Global

Fable.Core.JsInterop.importAll "../sass/main.sass"

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
                                            prop.value "bulma"
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

let loginLabel model dispatch ( name : string ) =
    Html.div
        [
            prop.className "field"
            prop.children
                [
                    Html.div
                        [
                            prop.className "label"
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
                            prop.className "row"
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

let root model dispatch =
    match model.CurrentPage with
    | Main msg ->
        Main.View.root model.Main ( Login.Types.MainMsg >>
                             dispatch)
    | Login -> logInRoot model dispatch

open Elmish.React
open Elmish.Debug
open Elmish.HMR

// App
Program.mkProgram init update root
|> Program.toNavigable (parseHash pageParser) urlUpdate
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReactSynchronous "elmish-app"
|> Program.run
