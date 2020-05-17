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
    | NewAddInfoMsg of array<ReactElement>
    | NewInstructionIdMsg of string
    | SpinnerVisibleMsg of IStyleAttribute
    | NewFilesChosenMsg of array<MediaChoiceFormData> * string
    | NewInstructionsListMsg of array<string>
    | NewCurrentInstructionMsg of option<Data.InstructionData option * string>
    | NewDataToInstruction of DatabaseNewFilesOptions * DBIds * Position
    | CreateNewDataMsg of
        SaveDataProgress<((Types.File * string) * DBIds * Position * DatabaseNewFilesOptions),array<DatabaseSavingOptions> * DBIds * Position>

type SearchResult =
    | Instruction of Data.InstructionData * Cmd<Instruction.Types.Msg>
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

