module User.Types

open Global
open Data

type Msg =
    | LoadedInstructions of AsyncOperationEvent<Result<UserData, string>>
    | UserDataMsg of string 
    | InstructionMsg of Instruction.State.Msg
    | InstructionSearchMsg of InstructionSearch.Types.Msg
    | CategorySearchMsg of Category.Types.Msg
    | NewAddMsg of NewAdd.Types.Msg

type Model =
    {
      Username : string
      Password : string
      Id : int
      CurrentPage: Global.UserPage
      InstructionSearch: InstructionSearch.Types.Model
      NewAdd : NewAdd.Types.Model
      Category : Category.Types.Model
      UserData : Data.Deferred<Result<UserData, string>>
      Instruction: Instruction.Types.Model
    }
