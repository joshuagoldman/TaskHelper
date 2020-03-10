module User.Types

open Global
open Data
open Controls
open Feliz
open Browser

type NewUserPage =
    | NoDelay of UserPage
    | Delay of UserPage * int

type Msg =
    | LoginAttemptMsg of string * string
    | LoadedInstructions of AsyncOperationEvent<Result<UserData, string>>
    | LoadedUsers of AsyncOperationEvent<Result<seq<LoginInfo>, string>>
    | UserDataMsg of string 
    | InstructionMsg of Instruction.Types.Msg
    | InstructionSearchMsg of InstructionSearch.Types.Msg
    | NewAddMsg of NewAdd.Types.Msg
    | LoginMessages of string
    | UserNameInputChangedMsg of string
    | PasswordInputChangedMsg of string
    | LoginSuceeded of UserData
    | NewUserId of int
    | LoadInstructions of UserData
    | LoginSpinnerMsg of IStyleAttribute
    | NewUserDataToAddMsg of Data.InstructionData
    | ChangePage of NewUserPage
    | NewAddNewCurrentInstruction of Option<string>
    | GiveResetInstruction of string
type Model =
    {
      AllUsers : Data.Deferred<Result<seq<LoginInfo>, string>>
      UserFromLogin : ValidationLoginInfo
      LoginMessage : string
      Id : int
      CurrentPage: Global.UserPage
      InstructionSearch: InstructionSearch.Types.Model
      NewAdd : NewAdd.Types.Model
      UserData : Data.Deferred<Result<UserData, string>>
      Instruction: Instruction.Types.Model
      LoginSpinner : AppearanceAttributes
    }
