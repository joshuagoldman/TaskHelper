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
    | CreateNewDataMsg of Result<seq<MediaChoiceFormData>,string>
    | NewInstructionIdMsg of string
    | PostInstruction of seq<MediaChoiceFormData>

type SearchResult =
    | Instruction of Data.InstructionData * Cmd<Instruction.Types.Msg>
    | Part of Data.partData * Cmd<Part.Types.Msg>

type Model =
    {
       NewInstructionData : Result<seq<MediaChoiceFormData>,string>
       NewAddMessages : string
       NewInstructionId : Option<string>
       LoadIcon : AppearanceAttributes
    }

