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

type DeleteResult =
    | DeleteSucceded of ReactElement
    | DeleteFailed of ReactElement

type DeleteProcess<'a,'b,'c> =
    | DeleteHasNotStartedYet of 'a
    | DeleteInProgress of 'b
    | DeleteFinished of DeleteResult

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
    | DeletePartFilesMsg of DeleteProcess<Data.partData * DBIds,ReactElement,DeleteResult>
    | ChangeFileStatus of PartStatus  * Position
    | SaveInstructionToDataBase of Position
    | CheckIfSaveFinished of Position
    | CreateDeletePopup of Position

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
