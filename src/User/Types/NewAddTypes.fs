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
    | CreateNewDataMsg of AsyncOperationEvent<Result<seq<MediaChoiceFormData>,string>>
    | NewInstructionIdMsg of string
    | PostInstruction of AsyncOperationEvent<Result<Data.InstructionData,string>>

type SearchResult =
    | Instruction of Data.InstructionData * Cmd<Instruction.Types.Msg>
    | Part of Data.partData * Cmd<Part.Types.Msg>

type Model =
    {
       NewInstructionData : Data.Deferred<Result<seq<MediaChoiceFormData>,string>>
       NewAddMessages : string
       NewInstructionId : int
       LoadIcon : AppearanceAttributes
    }

