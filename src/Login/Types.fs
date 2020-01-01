module Login.Types

open Controls
open Data
open Elmish

type Msg =
    | LoginButtonClicked of string * string
    | MainMsg of Main.Types.Msg

type Model =
    {
        CurrentPage : Global.Page
        Username : string
        Password : string
        Main : Main.Types.Model
    }
