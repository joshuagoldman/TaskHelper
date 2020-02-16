module Instruction.Types

open Controls
open Part
open Data
open Elmish
open Feliz
open Fable.React

type modificationInfo = {
    Position : option<int>
    IsChecked : option<bool>
    Name : option<string>
}

type NamePair = {
    CurrName : string
    NewName : string Option
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

type InstructionMode  =
| Regular
| Modification

type Model =
    {
        InstructionErrorMessage : AppearanceAttributes
        CurrInstruction : Result<Data.InstructionData,string>
        CurrPart : Part.Types.Model
        CurrPositions : Option<seq<modificationInfo>>
        PartNameModificationInput : AppearanceAttributes
        PositionsInput : AppearanceAttributes
        DeleteButton : AppearanceAttributes
    }
