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
open Data

let init() : Model * Cmd<Login.Types.Msg> =
    {
        UsernameLabel = { defaultAppearanceAttributes with Text = "UsernameLabel" }
        TextInput = defaultAppearanceAttributes
        LoginButton = { defaultAppearanceAttributes with Text = "LoginButton" }
    }, []



let update msg model : Model * Cmd<Login.Types.Msg> =
    match msg with
    | _ -> model, []

