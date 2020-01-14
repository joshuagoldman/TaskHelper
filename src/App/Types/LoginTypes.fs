module Login.Types

open Controls
open Data
open Elmish

type LoginAttempt =
    | Success 
    | Failed

type Msg =
   | Msg


type Model =
    {
        UsernameLabel : AppearanceAttributes
        TextInput : AppearanceAttributes
        LoginButton : AppearanceAttributes
    }
