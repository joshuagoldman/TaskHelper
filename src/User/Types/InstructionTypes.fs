module Instruction.Types

open Controls
open Part
open Data
open Elmish
open Feliz
open Fable.React
open Browser


type PartStatus =
    | Delete of string
    | Uploading of string
    | StatusExisting of string
    | UploadOrDeleteFinished of string * ReactElement

type DatabaseChangeResult =
    | DatabaseChangeSucceeded of ReactElement * Data.Position * seq<DatabaseSavingOptions> * DBIds
    | DatabaseChangeFailed of ReactElement * Data.Position

type DatabaseChangeProcess<'a,'b> =
    | DatabaseChangeBegun of 'a
    | DatabseChangeFinished of 'b

type DeleteProcess<'a,'b> =
    | DeleteInProgress of 'a
    | DeleteFinished of 'b

type DeleteInfo =
    | Delete of string
    | Regret of string

type NamePair = {
    CurrName : string
    NewName : string Option
}

type modificationInfo = {
    DelOrReg : option<DeleteInfo>
    Names : NamePair
    Status : seq<PartStatus>
}

type Msg =
    | NewInstruction2Show of Data.InstructionData * string
    | PartMsg of Part.Types.Msg
    | ErrorMsg of string
    | ModifyInstructionMsg of IStyleAttribute
    | DeleteButtonEnablenMsg of bool
    | NewModificationInfo of DeleteInfo Option *
                             string *
                             string Option
    | ImplementNewNames
    | UpdateNewName of string * string
    | NewFileAddMsg of ReactElement
    | ResetInstruction of string
    | NewPage of Global.Page * int Option
    | HoverPartMsg of Data.partData * Types.MouseEvent * IStyleAttribute
    | SaveData of Result<Data.InstructionData * string,string> *
                  option<seq<modificationInfo>> *
                  Position
    | DeletePartFilesMsg of DeleteProcess<string * Data.Position * Data.DBIds,string * Data.Position * ReactElement> 
    | ChangeFileStatus of PartStatus  * Position
    | SaveInstructionToDataBase of Position
    | CheckIfSaveFinished of Position
    | CheckIfDeleteFinished of Position
    | CreateDeletePopup of Position
    | DatabaseChangeMsg of DatabaseChangeProcess<seq<Data.DatabaseSavingOptions> * Data.DBIds * Data.Position,DatabaseChangeResult> 

type InstructionMode  =
| Regular
| Modification

type Model =
    {
        InstructionErrorMessage : AppearanceAttributes
        CurrInstruction : Result<Data.InstructionData * string,string>
        CurrPart : Part.Types.Model
        CurrPositions : Option<seq<modificationInfo>>
        CurrTempInstruction : Option<InstructionData>
        PartNameModificationInput : AppearanceAttributes
        PositionsInput : AppearanceAttributes
        DeleteButton : AppearanceAttributes
        FileAddMsg : AppearanceAttributes
    }
