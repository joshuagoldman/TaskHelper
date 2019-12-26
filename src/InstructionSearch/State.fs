module InstructionSearch.State

open Elmish
open Controls
open Instruction
open Part
open Types
open State

type Msg =
    | TextHasChanged of string
    | InstructionHasBeenClicked of Instruction.State.Msg
    | PartMsgFromSearch of Part.State.Msg
    | ClearSearchResult 

let init() : InstructionSearch.Types.Model * Cmd<Msg> =
  {
     SearchBar =  defaultAppearanceAttributes
     ResultFromSearch =
        seq [
                Instruction(Instruction.State.init() |> fun (a,b) -> a.Instruction, b)
                Part(Part.State.init() |> fun (a,b) -> a.Data, b)
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
    | PartMsgFromSearch msg ->
        let (part, partCmd) = Part.State.update msg model.Part
        { model with Part = part }, Cmd.map PartMsgFromSearch partCmd
    | ClearSearchResult -> { model with ResultFromSearch = [] ;
                                        SearchBar =
                                            { model.SearchBar with Text = "" } }, []
            
