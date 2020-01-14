module App.Logic

open Feliz
open App.Types
open Data
open Global

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

let loginToUserIfSuccess ( model : App.Types.Model ) =
    async {
        do! Async.Sleep 2000
        let page =
            match model.User.UserData with
            | Resolved response ->
                match response with
                | Ok _ -> User(UserPage.InstructionSearch)
                | Error _ -> Page.Login
            | _ -> Page.Login

        return (page |> Finished |> LoginToUser)


    }

let sleepAndLogout =
    async {
        do! Async.Sleep 3000
        return "You have been logged out due to inactivity ;)" |>
        Finished |> App.Types.InactivityMsg
    } 



