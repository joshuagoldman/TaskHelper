module InstructionSearch.Types

open Controls
open Part
open Elmish
open Types
open Data

type Msg =
    | TextHasChanged of string
    | ClearSearchResult

type SearchResult =
    | Instruction of Data.InstructionData * string * Cmd<Instruction.Types.Msg>
    | Part of Data.partData * Cmd<Part.Types.Msg> * Data.InstructionData * Cmd<Instruction.Types.Msg>

type Model =
    {
       SearchBar : AppearanceAttributes
       ResultFromSearch : array<Result<SearchResult,string>>
    }

