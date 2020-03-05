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
    Position : option<int>
    DelOrReg : option<DeleteInfo>
    Names : NamePair
}

type Msg =
    | NewInstruction2Show of Data.InstructionData
    | PartMsg of Part.Types.Msg
    | ErrorMsg of string
    | ModifyInstructionMsg of IStyleAttribute
    | DeleteButtonEnablenMsg of bool
    | NewModificationInfo of int Option *
                             bool Option *
                             NamePair
    | NewCurrPositions of Data.InstructionData
    | NewFileAddMsg of ReactElement

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
