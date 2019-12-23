module InstructionSearch.State

open Elmish
open Controls
open Instruction
open Part
open Types
open State

type Msg =
    | TextHasChanged of string
    | InstructionHasBeenClicked of Instruction.Types.Msg
    | PartHasBeenClicked of Part.State.Msg

let init() : InstructionSearch.Types.Model * Cmd<Msg> =
  {
     SearchBar =  defaultAppearanceAttributes
     ResultFromSearch =
        seq [
                Instruction(Instruction.State.init())
                Part(Part.State.init())
            ]
     Instruction = Instruction.State.init() |> fun (a,_) -> a
     Part = Part.State.init() |> fun (a,_) -> a
  }, []

let update msg model =
    match msg with
    | TextHasChanged str ->
        {model with SearchBar = {model.SearchBar with Text = str }}, []
    | InstructionHasBeenClicked msg ->
        let (newInstruction, newInstructionCmd) =
            Instruction.State.update msg model.Instruction
        { model with Instruction = newInstruction }, Cmd.map InstructionHasBeenClicked newInstructionCmd
    | PartHasBeenClicked msg ->
        let (part, partCmd) = Part.State.update msg model.Part
        { model with Part = part }, Cmd.map PartHasBeenClicked partCmd
            
