module Login.Logic

open Feliz
open Types
open Data

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
    | "password" -> txt |> (User.Types.PasswordInputChangedMsg >> App.Types.UserMsg >> dispatch)
    | "text" -> txt |> (User.Types.UserNameInputChangedMsg >> App.Types.UserMsg >> dispatch)
    | _  -> txt |> (User.Types.PasswordInputChangedMsg >> App.Types.UserMsg >> dispatch)

let loadClientData =
    "" |> fun _ -> ()



