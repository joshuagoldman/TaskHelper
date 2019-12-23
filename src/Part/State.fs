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
        Title = "Example instruction"
        Video = Some "Videos/Video1.mp4"
        InstructionTxt =Some "This is some info that'll be available for the user, so that the user'll know what to do"
    }, []

let update msg model : Model * Cmd<Msg> =
    match msg with
    | NewPart2Show newModel -> newModel, []
    | Joshua str -> {model with Video = Some str}, []

