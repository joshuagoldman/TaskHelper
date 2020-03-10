module Instruction.Types

open Controls
open Part
open Data
open Elmish
open Feliz
open Fable.React

type DeleteInfo =
    | Delete of string
    | Regret of string

type NamePair = {
    CurrName : string
    NewName : string Option
    NameToChangeTo : string Option
}

type modificationInfo = {
    DelOrReg : option<DeleteInfo>
    Names : NamePair
}

type ResetActions =
    | ResetInstructionNotObtained of string
    | ResetInstructionObtained of Data.InstructionData

type Msg =
    | NewInstruction2Show of Data.InstructionData
    | PartMsg of Part.Types.Msg
    | ErrorMsg of string
    | ModifyInstructionMsg of IStyleAttribute
    | DeleteButtonEnablenMsg of bool
    | NewModificationInfo of DeleteInfo Option *
                             NamePair
    | NewName of string * string
    | UpdateNewName of string * string
    | NewFileAddMsg of ReactElement
    | Reset of ResetActions

type InstructionMode  =
| Regular
| Modification

type Model =
    {
        InstructionErrorMessage : AppearanceAttributes
        CurrInstruction : Result<Data.InstructionData,string>
        CurrPart : Part.Types.Model
        CurrPositions : Option<seq<modificationInfo>>
        CurrTempInstruction : Option<InstructionData>
        PartNameModificationInput : AppearanceAttributes
        PositionsInput : AppearanceAttributes
        DeleteButton : AppearanceAttributes
        FileAddMsg : AppearanceAttributes
    }
