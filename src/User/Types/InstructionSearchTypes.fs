module InstructionSearch.Types

open Controls
open Part
open Elmish
open Types
open Data

type SearchResult<'a> =
    | Instruction of Data.InstructionData * string * Cmd<Instruction.Types.Msg<'a>>
    | Part of Data.partData * Cmd<Part.Types.Msg> * Data.InstructionData * Cmd<Instruction.Types.Msg<'a>>

type Msg<'a> =
    | TextHasChanged of string
    | ClearSearchResult
    | GetNewInstruction of array<Result<SearchResult<'a>,string>>

type Model<'a> =
    {
       SearchBar : AppearanceAttributes
       ResultFromSearch : array<Result<SearchResult<'a>,string>>
    }

