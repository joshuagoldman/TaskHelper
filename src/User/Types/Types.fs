module User.Types

open Global
open Data

type Msg =
    | LoadedInstructions of AsyncOperationEvent<Result<seq<InstructionData>, string>>
    | InstructionMsg of Instruction.State.Msg
    | InstructionSearchMsg of InstructionSearch.Types.Msg
    | CategorySearchMsg of Category.Types.Msg

type Model =
    {
      Username : string
      Password : string
      CurrentPage: Global.UserPage
      InstructionSearch: InstructionSearch.Types.Model
      Category : Category.Types.Model
      UserData : Data.Deferred<Result<seq<InstructionData>, string>>
      Instruction: Instruction.Types.Model
    }
