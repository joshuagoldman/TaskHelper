module Instruction.Types

open Controls
open Part
open Data
open Elmish

type Model =
    {
        CurrInstruction : Data.InstructionData
        CurrPart : Part.Types.Model
    }
