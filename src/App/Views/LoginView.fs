module Login.View

open Feliz
open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Types
open Global
open Data
open Browser
   
let loginButton ( model : App.Types.Model ) dispatch =
    Html.div[
        prop.className "columns is-centered"
        prop.children[
            Html.a[
                prop.className "button"
                prop.style[
                        style.backgroundColor.blue
                        style.fontStyle.italic
                        style.color.black
                        style.fontWeight.bold
                        style.fontSize 25
                        style.opacity 0.9
                        style.borderRadius 10
                        style.margin(20,400,400,400)
                    ]

                prop.onClick (fun _ ->
                            User.Logic.loginAttempt model.User HasNostStartedYet
                            |> Seq.map (fun msg -> msg |> App.Types.UserMsg)
                            |> Seq.iter (fun msg -> ( msg |> dispatch))
                )

                prop.children[
                        Fable.React.Helpers.str "Login"
                ]
            ]
        ]
    ]

let loginText model dispatch txtType =

    Html.div[
        prop.className "columns is-centered"
        prop.children[
            Html.div[
                prop.className "field"
                prop.onChange (fun txt ->
                        App.Logic.loginInfoChanged dispatch txt txtType)
                prop.children[
                    Html.div[
                        prop.className "control has-icons-left has-icons-right"
                        prop.style[
                                style.margin(20,400,0,400)
                        ]
                        prop.children[
                            Html.input[
                                prop.className "input is-success"
                                prop.type' txtType
                                prop.placeholder "Text input"
                            ]
                            Html.span[
                                prop.className "icon is-small is-left"
                                prop.children[
                                    Html.i[
                                            prop.className "fas fa-user"
                                    ]
                                ]
                            ]
                        ] 
                    ]

                ]
            ]
        ]
    ]

let loginLabel model dispatch name =
    Html.div[
        prop.className "columns is-centered"
        prop.children[
            Html.div[
                prop.className "field"
                prop.children[
                    Html.div[
                        prop.className "label"
                        prop.style[
                            style.justifyContent.center
                            style.display.flex
                            style.fontSize 30.1
                        ]
                        prop.children[
                            Fable.React.Helpers.str name
                        ] 
                    ]

                ]
            ]
        ]
    ]

    
let spinnerVisible ( visible : IStyleAttribute ) =
    visible
    |> function
        | _  when visible = style.visibility.hidden ->
            Html.none       
        | _ ->
           Html.div[
               prop.className "columns is-centered"
               prop.style[
                   style.marginTop 15
                   style.color.black
               ]
               prop.children[
                   Html.i[
                       prop.className "fa fa-cog fa-spin fa-2x"
                   ]
               ]
           ]

let loginTxtArea ( model : App.Types.Model ) dispatch =

    Html.div
        [
            prop.className "columns is-centered"
            prop.style[
                style.marginTop 10
                style.color.black
                style.fontWeight.bold
            ]
            prop.children
                [
                    Fable.React.Helpers.str model.User.LoginMessage
                ]
        ]


let root ( model : App.Types.Model ) dispatch =
    Html.div[
        prop.style[
            style.backgroundImage "url(img/Beach_Img.jpg)"
            style.backgroundSize "100% 100%"
        ]
        prop.children[
            loginLabel model dispatch "User name"
            loginText model dispatch "text"
            loginLabel model dispatch "Password"
            loginText model dispatch "password"
            loginTxtArea model dispatch
            spinnerVisible model.User.LoginSpinner.Visible
            loginButton model dispatch
        ]
    ]
