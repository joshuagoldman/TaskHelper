module Instruction.State

open Elmish
open Controls
open Types
open Part
open Data

type Msg =
    | NewInstruction2Show of Data.InstructionData
    | PartMsg of Part.State.Msg
    | ErrorMsg of string

let init () : Model * Cmd<Msg> =

    {
      InstructionErrorMessage = defaultAppearanceAttributes
      CurrInstruction = Error ""
      CurrPart = Part.State.init() |> fun (a,b) -> a
    }, []


let update msg model : Instruction.Types.Model * Cmd<Msg>  =
    match msg with
    | NewInstruction2Show instruction ->
        { model with CurrInstruction = Ok instruction }, []
    | PartMsg msg ->
        let (parModel, partModelCmd) = Part.State.update msg model.CurrPart
        { model with CurrPart = parModel}, Cmd.map PartMsg partModelCmd
    | ErrorMsg str ->
        { model with InstructionErrorMessage =
                        { model.InstructionErrorMessage with Text = str} }, []
