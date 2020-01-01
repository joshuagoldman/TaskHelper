module Login.Logic

open Feliz

let decideMargin name =
    match name with
    | "UsernameInput" -> style.marginTop 40
    | "PasswordInput" -> style.margin 40
    | "UsernameLabel" ->  style.margin 200
    | "PasswordLabel" ->  style.margin 40
    | "LoginButton" -> style.marginTop 300
    | _ -> style.margin 40
