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
    | NewInstructionIdMsg of string
    | SpinnerVisibleMsg of IStyleAttribute
    | NewFilesChosenMsg of seq<MediaChoiceFormData> * string
    | NewInstructionsListMsg of seq<string>
    | NewCurrentInstructionMsg of option<Data.InstructionData option * string>
    | SaveNewData of Data.InstructionData * DBIds * Position
    | CheckIfSaveFinished of DBIds * Position
    | CreateNewDataMsg of
        AsyncOperationSavingStatus<SaveDataProgress<(Types.File * DBIds * Position),seq<DatabaseSavingOptions> * DBIds * Position>>

type SearchResult =
    | Instruction of Data.InstructionData * Cmd<Instruction.Types.Msg>
    | Part of Data.partData * Cmd<Part.Types.Msg>

type Model =
    {
       NewInstructionData : Option<seq<MediaChoiceFormData>>
       NewAddMessages : seq<ReactElement>
       LoadIcon : AppearanceAttributes
       VideosUploadInput : AppearanceAttributes
       InstructionTxtUploadInput : AppearanceAttributes
       InstructionList : Option<seq<string>>
       CurrentInstruction : Option<Data.InstructionData option * string>
    }

