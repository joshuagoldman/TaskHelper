module Login.Types

open Controls
open Data
open Elmish
open Feliz

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
