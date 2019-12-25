module App.Types

open Global

type Msg =
    | PartMsg of Part.State.Msg
    | InstructionSearchMsg of InstructionSearch.State.Msg
    | InstructionMsg of Instruction.State.Msg

type Model =
    {
      CurrentPage: Page
      InstructionSearch: InstructionSearch.Types.Model
      Instruction: Instruction.Types.Model
      Part : Part.Types.Model
    }
