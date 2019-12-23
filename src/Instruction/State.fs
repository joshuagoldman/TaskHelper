module Instruction.State

open Elmish
open Controls
open Types
open Part

let init () : Model * Cmd<Msg> =

    {
        Parts =
            seq[
                    {
                        Control = defaultAppearanceAttributes
                        Data = Part.State.init()
                    }
            ]

        Title = ""
    }, []


let update msg model : Instruction.Types.Model * Cmd<Msg>  =
    match msg with
    | NewInstruction2Show newModel -> newModel, []
