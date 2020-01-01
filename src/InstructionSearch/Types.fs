module InstructionSearch.Types

open Controls
open Part
open Elmish
open Types
open Data

type SearchResult =
    | Instruction of Data.InstructionData * Cmd<Instruction.State.Msg>
    | Part of Data.partData * Cmd<Part.State.Msg>

type Model =
    {
       SearchBar : AppearanceAttributes
       allInstructions : seq<Instruction.Types.Model>
       ResultFromSearch : seq<SearchResult>
       Instruction: Instruction.Types.Model
       Part : Part.Types.Model
    }

