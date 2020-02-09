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

type IsUploading =
| Yes of ReactElement
| No of ReactElement

type MediaChoiceFormData =
    | Video of Types.File * IsUploading
    | InstructionTxt of Types.File * IsUploading

type Msg =
    | NewAddInfoMsg of seq<ReactElement>
    | CreateNewDataMsg of AsyncOperationSavingStatus<SaveDataProgress<MediaChoiceFormData>>
    | NewInstructionIdMsg of string
    | PostInstruction of seq<MediaChoiceFormData>
    | NewFilesChosenMsg of seq<MediaChoiceFormData> * string
    | SpinnerVisibleMsg of IStyleAttribute
    | ChangeFileStatus of MediaChoiceFormData * IsUploading

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
    }

