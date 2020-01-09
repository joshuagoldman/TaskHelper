module Login.Types

open Controls
open Data
open Elmish

type LoginAttempt =
    | Success 
    | Failed

type Msg =
    | LoginButtonClicked of string * string
    | UsernameCHanged of string
    | PasswordChanged of string
    | LoginAttemptMsg of LoginAttempt 

type Model =
    {
        Username : string
        Password : string
        UsernameLabel : AppearanceAttributes
        TextInput : AppearanceAttributes
        LoginButton : AppearanceAttributes
    }
