module Login.Logic

open Feliz
open Types

let decideMargin name =
    match name with
    | "UsernameInput" -> style.marginTop 20
    | "PasswordInput" -> style.margin 20
    | "UsernameLabel" ->  style.margin 20
    | "PasswordLabel" ->  style.margin 20
    | "LoginButton" -> style.marginTop 100
    | _ -> style.margin 20


let loginInfoChanged dispatch txt txtType =
    match txtType with
    | "password" -> txt |> (PasswordChanged >> App.Types.LoginMsg >> dispatch)
    | "text" -> txt |> (UsernameCHanged >> App.Types.LoginMsg >> dispatch)
    | _  -> txt |> (UsernameCHanged >> App.Types.LoginMsg >> dispatch)

let loadClientData =
    "" |> fun _ -> ()
