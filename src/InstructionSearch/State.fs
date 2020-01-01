module InstructionSearch.State

open Elmish
open Controls
open Types

type Msg =
    | TextHasChanged of string
    | InstructionMsgIS of Instruction.State.Msg
    | PartMsgIS of Part.State.Msg
    | ClearSearchResult 

let init() : InstructionSearch.Types.Model * Cmd<Msg> =
  {
     SearchBar =  defaultAppearanceAttributes
     allInstructions =
        seq [
                Instruction.State.init() |> fun (a,_) -> a
            ]
     ResultFromSearch =
        seq [
                Instruction(Instruction.State.init() |> fun (a,b) -> a.Instruction, b)
                Part(Part.State.init() |> fun (a,b) -> a.Data, b)
            ]
     Instruction = Instruction.State.init() |> fun (a,_) -> a
     Part = Part.State.init() |> fun (a,_) -> a
  }, []

let update msg model : Model * Cmd<Msg> =
    match msg with
    | TextHasChanged str ->
        {model with SearchBar = {model.SearchBar with Text = str }}, []
    | InstructionMsgIS msg ->
        let (newInstruction, newInstructionCmd) =
            Instruction.State.update msg model.Instruction
        { model with Instruction = newInstruction }, Cmd.map InstructionMsgIS newInstructionCmd
    | PartMsgIS msg ->
        let (part, partCmd) = Part.State.update msg model.Part
        { model with Part = part }, Cmd.map PartMsgIS partCmd
    | ClearSearchResult -> { model with ResultFromSearch = [] ;
                                        SearchBar =
                                            { model.SearchBar with Text = "" } }, []
            
