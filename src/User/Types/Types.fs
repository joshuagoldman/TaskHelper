module User.Types

open Global
open Data

type Msg =
    | LoginAttemptMsg of string * string
    | LoadedInstructions of AsyncOperationEvent<Result<UserData, string>>
    | LoadedUsers of AsyncOperationEvent<Result<seq<LoginInfo>, string>>
    | UserDataMsg of string 
    | InstructionMsg of Instruction.State.Msg
    | InstructionSearchMsg of InstructionSearch.Types.Msg
    | CategorySearchMsg of Category.Types.Msg
    | NewAddMsg of NewAdd.Types.Msg
    | LoginMessages of string
    | UserNameInputChangedMsg of string
    | PasswordInputChangedMsg of string
    | LoginSuceeded
    | NewUserId of int
    | LoadInstructions of UserData
type Model =
    {
      AllUsers : Data.Deferred<Result<seq<LoginInfo>, string>>
      UserFromLogin : ValidationLoginInfo
      LoginMessage : string
      Id : int
      CurrentPage: Global.UserPage
      InstructionSearch: InstructionSearch.Types.Model
      NewAdd : NewAdd.Types.Model
      Category : Category.Types.Model
      UserData : Data.Deferred<Result<UserData, string>>
      Instruction: Instruction.Types.Model
    }
