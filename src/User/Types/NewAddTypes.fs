module NewAdd.Types

open Controls
open Part
open Elmish
open Types
open Data
open Browser
open Browser.Blob
open Fable.React
open Feliz

type MediaChoiceFormData =
    | Video of Types.File
    | InstructionTxt of Types.File

type Msg =
    | NewAddInfoMsg of seq<ReactElement>
    | CreateNewDataMsg of Result<seq<MediaChoiceFormData>,string>
    | NewInstructionIdMsg of string
    | PostInstruction of seq<MediaChoiceFormData>
    | NewFilesChosenMsg of seq<MediaChoiceFormData> * string
    | ProgressBarVisibleMsg of IStyleAttribute

type SearchResult =
    | Instruction of Data.InstructionData * Cmd<Instruction.Types.Msg>
    | Part of Data.partData * Cmd<Part.Types.Msg>

type Model =
    {
       NewInstructionData : Option<seq<MediaChoiceFormData>>
       NewAddMessages : seq<ReactElement>
       NewInstructionId : Option<string>
       LoadIcon : AppearanceAttributes
       VideosUploadInput : AppearanceAttributes
       InstructionTxtUploadInput : AppearanceAttributes
       Progressbar : AppearanceAttributes
    }

