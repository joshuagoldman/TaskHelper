module Login.Logic

open Feliz

let decideMargin name =
    match name with
    | "UsernameInput" -> style.marginTop 20
    | "PasswordInput" -> style.margin 20
    | "UsernameLabel" ->  style.margin 20
    | "PasswordLabel" ->  style.margin 20
    | "LoginButton" -> style.marginTop 100
    | _ -> style.margin 20
