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
}

type modificationInfo = {
    DelOrReg : option<DeleteInfo>
    Names : NamePair
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