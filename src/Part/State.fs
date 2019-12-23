module Part.State

open Controls
open Types
open Elmish

type Msg =
    | NewPart2Show of Model
    | Joshua of string

let init () : Model * Cmd<Msg> =
    {
        NextButton = defaultAppearanceAttributes
        PreviousButton = defaultAppearanceAttributes
        GoBackButton = defaultAppearanceAttributes
        Instruction = "instruction"
        Title = "PartTitle"
        Video = Some ""
        InstructionTxt =Some ""
    }, []

let update msg model : Model * Cmd<Msg> =
    match msg with
    | NewPart2Show newModel -> newModel, []
    | Joshua str -> {model with Video = Some str}, []

