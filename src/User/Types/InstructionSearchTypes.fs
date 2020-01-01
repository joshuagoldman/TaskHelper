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
    | Instruction of Data.InstructionData * Cmd<Instruction.State.Msg>
    | Part of Data.partData * Cmd<Part.State.Msg>

type Model =
    {
       SearchBar : AppearanceAttributes
       ResultFromSearch : seq<SearchResult>
    }

