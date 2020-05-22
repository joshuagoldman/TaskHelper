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

type Msg<'a> =
    | NewAddInfoMsg of array<ReactElement>
    | NewInstructionIdMsg of string
    | SpinnerVisibleMsg of IStyleAttribute
    | NewFilesChosenMsg of array<MediaChoiceFormData> * string
    | NewInstructionsListMsg of array<string>
    | NewCurrentInstructionMsg of option<Data.InstructionData option * string>
    | NewDataToInstruction of DatabaseNewFilesOptions * DBIds * Utilities<'a>
    | CreateNewDataMsg of
        SaveDataProgress<((Types.File * string) * DBIds * Utilities<'a> * DatabaseNewFilesOptions),array<DatabaseSavingOptions> * DBIds * Utilities<'a>>

type SearchResult<'a> =
    | Instruction of Data.InstructionData * Cmd<Instruction.Types.Msg<'a>>
    | Part of Data.partData * Cmd<Part.Types.Msg>

type Model =
    {
       NewInstructionData : Option<array<MediaChoiceFormData>>
       NewAddMessages : array<ReactElement>
       LoadIcon : AppearanceAttributes
       VideosUploadInput : AppearanceAttributes
       InstructionTxtUploadInput : AppearanceAttributes
       InstructionList : Option<array<string>>
       CurrentInstruction : Option<Data.InstructionData option * string>
    }

