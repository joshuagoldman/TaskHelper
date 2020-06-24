module Instruction.Types

open Controls
open Part
open Data
open Elmish
open Feliz
open Fable.React
open Browser

type Uploaded =
      | Percentage of float
      | NoneUploaded

type PartStatus =
    | Delete of string
    | DeleteFinished of string * ReactElement
    | Uploading of string * Uploaded
    | StatusExisting of string
    | UploadFinishedSuccesfully of string * ReactElement
    | UploadFinishedWithFailure of string * ReactElement

type DatabaseChangeResult<'a> =
    | DatabaseChangeSucceeded of ReactElement * Data.Utilities<'a> * array<DatabaseSavingOptions>
    | DatabaseChangeFailed of ReactElement * Data.Utilities<'a>

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
    Status : array<PartStatus>
}

type Msg<'a> =
    | NewInstruction2Show of Data.InstructionData * string
    | PartMsg of Part.Types.Msg
    | ErrorMsg of string
    | ModifyInstructionMsg of IStyleAttribute
    | DeleteButtonEnablenMsg of bool
    | NewModificationInfo of DeleteInfo Option *
                             string *
                             string Option
    | ImplementNewNames of Utilities<'a>
    | UpdateNewName of string * string
    | UpdateNewInstructionName of InstructionTitleInfo
    | ResetInstruction of string
    | NewPage of Global.Page * int Option
    | HoverPartMsg of Data.partData * Data.Utilities<'a> * IStyleAttribute
    | DeletePartFilesMsg of DeleteProcess<string * Data.Utilities<'a>,string * Data.Utilities<'a> * ReactElement> 
    | ChangeFileStatus of PartStatus  * Utilities<'a>
    | SaveInstructionToDataBase of Utilities<'a>
    | CheckIfSaveFinished of DBIds * Utilities<'a> * DatabaseNewFilesOptions
    | CreateDeletePopup of Utilities<'a>
    | DatabaseChangeMsg of DatabaseChangeProcess<array<Data.DatabaseSavingOptions> * Data.DBIds * Data.Utilities<'a>,DatabaseChangeResult<'a>>

type InstructionMode  =
| Regular
| Modification

type Model<'a> =
    {
        InstructionErrorMessage : AppearanceAttributes
        CurrInstruction : Result<Data.InstructionData * string,string>
        CurrPart : Part.Types.Model
        CurrPositions : Option<array<modificationInfo>>
        CurrTempInstruction : Option<InstructionData>
        PartNameModificationInput : AppearanceAttributes
        PositionsInput : AppearanceAttributes
        DeleteButton : AppearanceAttributes
        FileAddMsg : AppearanceAttributes
        UserTypeDispatch : UsrTypeDispatchOptions<'a>
    }
