module Instruction.Types

open Controls
open Part
open Data
open Elmish

type Msg =
    | NewInstruction2Show of Data.InstructionData
    | PartMsg of Part.Types.Msg
    | ErrorMsg of string

type Model =
    {
        InstructionErrorMessage : AppearanceAttributes
        CurrInstruction : Result<Data.InstructionData,string>
        CurrPart : Part.Types.Model
    }
