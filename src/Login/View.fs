module Login.View

open Feliz
open State
open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Types
open Global

//Fable.Core.JsInterop.importAll "../sass/main.sass"
let loginButton model dispatch =
    Html.a
        [
            prop.className "button"
            prop.style
                [
                    style.backgroundColor.blueViolet
                    Logic.decideMargin model.LoginButton.Text
                    style.fontSize 25
                    style.opacity 0.9
                    style.borderRadius 10
                ]
            prop.href (Global.toHashUser UserPage.InstructionSearch )
            prop.children
                [
                    Fable.React.Helpers.str "Login"
                ]
        ]
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
                                    Logic.decideMargin model.UsernameLabel.Text
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

let root model dispatch =
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
                                    loginLabel model dispatch "User name"
                                    loginText model dispatch "User name"
                                    loginLabel model dispatch "Password"
                                    loginText model dispatch "Password"
                                    loginButton model dispatch
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
