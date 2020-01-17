module App.Types

open Controls
open Data
open Elmish

type Msg =
    | UserMsg of User.Types.Msg
    | LoginMsg of Login.Types.Msg
    | LoginToUser of AsyncOperationEvent<Global.Page>  

type Model =
    {
        CurrentPage : Global.Page
        Login : Login.Types.Model
        User : User.Types.Model
    }
