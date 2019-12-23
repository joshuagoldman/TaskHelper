module InstructionSearch.Types

open Controls
open Part
open Elmish
open Types

type SearchResult =
    | Instruction of Instruction.Types.Model * Cmd<Instruction.Types.Msg>
    | Part of Part.Types.Model * Cmd<Part.State.Msg>

type Model =
    {
       SearchBar : AppearanceAttributes
       ResultFromSearch : seq<SearchResult>
       Instruction : Instruction.Types.Model
       Part : Part.Types.Model 
    }

let allPartsAndInstructions = 
    seq
        [
            Part(Part.State.init())

            Instruction(Instruction.State.init())
             
        ]
