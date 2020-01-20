module NewAdd.Types

open Controls
open Part
open Elmish
open Types
open Data
open Browser

type Msg =
    | NewAddMsg of string
    | CreateNewDataMsg of AsyncOperationEvent<Result<Data.UserData, string>>

type SearchResult =
    | Instruction of Data.InstructionData * Cmd<Instruction.State.Msg>
    | Part of Data.partData * Cmd<Part.State.Msg>

type Model =
    {
       NewInstructionData : Result<UserData,string>
       NewAddMessages : string
       LoadIcon : AppearanceAttributes
    }

