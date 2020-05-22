module App.Logic

open Feliz
open App.Types
open Data
open Global
open Browser

let decideMargin name =
    match name with
    | "UsernameInput" -> style.marginTop 20
    | "PasswordInput" -> style.margin 20
    | "UsernameLabel" ->  style.margin 20
    | "PasswordLabel" ->  style.margin 20
    | "LoginButton" -> style.marginTop 100
    | _ -> style.margin 20


let loginInfoChanged ( dispatch : Msg -> unit ) txt txtType =
    match txtType with
    | "password" ->
        txt |> (User.Types.PasswordInputChangedMsg >> App.Types.UserMsg >> dispatch)
    | "text" ->
        txt |> (User.Types.UserNameInputChangedMsg >> App.Types.UserMsg >> dispatch)
    | _  -> txt |> (User.Types.PasswordInputChangedMsg >> App.Types.UserMsg >> dispatch)

let loginToUserIfSuccess ( model : App.Types.Model ) = 
        let page =
            match model.User.UserData with
            | Resolved response ->
                match response with
                | Ok _ ->  User(UserPage.InstructionSearch) |> AsyncOperationEvent.Finished |> LoginToUser
                | Error _ -> Page.Login |> AsyncOperationEvent.Finished |> LoginToUser
            | _ -> Page.Login |> AsyncOperationEvent.Finished |> LoginToUser

        page



