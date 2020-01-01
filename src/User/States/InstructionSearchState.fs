module InstructionSearch.State

open Elmish
open Controls
open Types

let init() : InstructionSearch.Types.Model * Cmd<Msg> =
  {
     SearchBar =  defaultAppearanceAttributes
     ResultFromSearch =
        seq [
                Instruction(Instruction.State.init() |> fun (a,b) -> a.CurrInstruction, b)
                Part(Part.State.init() |> fun (a,b) -> a.Data, b)
            ]
  }, []

let update msg model : Model * Cmd<Msg> =
    match msg with
    | TextHasChanged str ->
        {model with SearchBar = {model.SearchBar with Text = str }}, []
    | ClearSearchResult -> { model with ResultFromSearch = [] ;
                                        SearchBar =
                                            { model.SearchBar with Text = "" } }, []
            
