module Instruction.Types

open Controls
open Part
open Data
open Elmish

type Model =
    {
        InstructionErrorMessage : AppearanceAttributes
        CurrInstruction : Result<Data.InstructionData,string>
        CurrPart : Part.Types.Model
    }
