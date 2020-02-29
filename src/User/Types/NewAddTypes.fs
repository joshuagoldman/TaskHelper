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
    | YesSuceeded of ReactElement

type ModificationType =
    | Add of Data.InstructionData
    | New

type MediaChoiceFormData =
    | Video of Types.File * IsUploading * ModificationType
    | InstructionTxt of Types.File * IsUploading * ModificationType

type Msg =
    | NewAddInfoMsg of seq<ReactElement>
    | NewInstructionIdMsg of string
    | SpinnerVisibleMsg of IStyleAttribute
    | NewFilesChosenMsg of seq<MediaChoiceFormData> * string
    | CreateNewDataMsg of
        AsyncOperationSavingStatus<SaveDataProgress<(MediaChoiceFormData * string)
            ,option<seq<MediaChoiceFormData>>>>
    | PostInstruction of seq<MediaChoiceFormData>
    | ChangeFileStatus of MediaChoiceFormData * IsUploading
    | NewInstructionsListMsg of seq<string>
    | NewCurrentInstructionMsg of Data.InstructionData Option

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
       InstructionList : Option<seq<string>>
       CurrentInstruction : Option<Data.InstructionData>
    }

