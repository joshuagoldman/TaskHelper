module NewAdd.Types

open Controls
open Part
open Elmish
open Types
open Data
open Browser
open Browser.Blob

type MediaChoiceFormData =
    | Video of Types.File
    | InstructionTxt of Types.File

type Msg =
    | NewAddInfoMsg of string
    | CreateNewDataMsg of AsyncOperationEvent<Result<Data.UserData, string>>

type SearchResult =
    | Instruction of Data.InstructionData * Cmd<Instruction.Types.Msg>
    | Part of Data.partData * Cmd<Part.Types.Msg>

type Model =
    {
       NewInstructionData : Result<Data.InstructionData,string>
       NewAddMessages : string
       LoadIcon : AppearanceAttributes
    }

