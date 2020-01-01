module App.Types

open Controls
open Data
open Elmish

type Msg =
    | UserMsg of User.Types.Msg

type Model =
    {
        CurrentPage : Global.Page
        Login : Login.Types.Model
        User : User.Types.Model
    }
