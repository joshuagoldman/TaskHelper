module Category.State

open Elmish
open Controls
open Types
open Part
open Data

let init () : Model * Cmd<Msg> =
    {
        Blocks =
            seq
                [
                    defaultAppearanceAttributes
                ]
    },[]


let update msg model : Category.Types.Model * Cmd<Msg>  =
    match msg with
    | NewInstruction2Show instruction -> model, []
