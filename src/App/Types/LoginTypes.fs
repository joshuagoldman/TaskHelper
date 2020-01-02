module Login.Types

open Controls
open Data
open Elmish

type Msg =
    | LoginButtonClicked of string * string
    | UsernameCHanged of string
    | PasswordChanged of string

type Model =
    {
        Username : string
        Password : string
        UsernameLabel : AppearanceAttributes
        TextInput : AppearanceAttributes
        LoginButton : AppearanceAttributes
    }
