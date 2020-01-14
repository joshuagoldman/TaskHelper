module Login.View

open Feliz
open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Types
open Global
open Data


let loginButton ( model : App.Types.Model ) dispatch =
    Html.a
        [
            prop.className "button"
            prop.style
                [
                    style.backgroundColor.blue
                    style.fontStyle.italic
                    style.color.black
                    style.fontWeight.bold
                    style.fontSize 25
                    style.opacity 0.9
                    style.borderRadius 10
                    style.width 200
                    style.left 200
                    style.margin(40,400,400,400)
                ]
            prop.href ( App.Types.LoginToUser Started |> dispatch
                        toHash model.CurrentPage )
            prop.onClick (fun _ ->  User.Logic.loginAttempt model.User HasNostStartedYet
                                    |> Seq.iter (fun msg -> msg |> (App.Types.UserMsg >> dispatch)))
            prop.children
                [
                    Fable.React.Helpers.str "Login"
                ]
        ]

let loginText model dispatch txtType =
    Html.div
        [
            prop.className "field"
            prop.onTextChange (fun txt -> App.Logic.loginInfoChanged dispatch txt txtType)
            prop.children
                [
                    Html.div
                        [
                            prop.className "control has-icons-left has-icons-right"
                            prop.style
                                [
                                    style.margin(20,400,0,400)
                                ]
                            prop.children
                                [
                                    Html.input
                                        [
                                            prop.className "input is-success"
                                            prop.type' txtType
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
    

let loginTxtArea ( model : App.Types.Model ) dispatch =

    Html.div
        [
            prop.className "textarea"
            prop.style
                [
                    style.backgroundColor.transparent
                    style.color.white
                    style.justifyContent.center
                    style.display.flex
                    style.fontSize 20.1
                    style.borderStyle.none
                    style.boxShadow.none
                    style.overflow.auto
                ]
            prop.children
                [
                    Fable.React.Helpers.str model.User.LoginMessage
                ]
        ]


let root ( model : App.Types.Model ) dispatch =
    Html.div
        [
            prop.style
                [
                    style.backgroundImage "url(img/Beach_Img.jpg)"
                    style.backgroundSize "100% 100%"
                ]
            prop.children
                [
                    Html.div
                        [
                            prop.children
                                [
                                    loginLabel model dispatch "User name"
                                    loginText model dispatch "text"
                                    loginLabel model dispatch "Password"
                                    loginText model dispatch "password"
                                    loginTxtArea model dispatch
                                    loginButton model dispatch
                                ]
                        ]
                ]
        ]
