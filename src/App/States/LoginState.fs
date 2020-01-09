module Login.State

open Controls
open Elmish
open Feliz
open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Browser
open Global
open Types
open Browser

let init() : Model * Cmd<Login.Types.Msg> =
    {
        Username = ""
        Password = ""
        UsernameLabel = { defaultAppearanceAttributes with Text = "UsernameLabel" }
        TextInput = defaultAppearanceAttributes
        LoginButton = { defaultAppearanceAttributes with Text = "LoginButton" }
    }, []



let update msg model : Model * Cmd<Login.Types.Msg> =
    match msg with
    | LoginButtonClicked (usrName,passWord) ->
        { model with Username = usrName ; Password = passWord}, []
    | UsernameCHanged usrName -> { model with Username = usrName }, []
    | PasswordChanged passWord -> { model with Password = passWord }, []
    | LoginAttemptMsg result ->

