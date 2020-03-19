module User.Types

open Global
open Data
open Controls
open Feliz
open Browser

type NewUserPage =
    | NoDelay of UserPage
    | Delay of UserPage * int

type PopUpSettings<'t> =
    | DefaultWithMsg of seq<ReactElement> * 't
    | OptionalWithMsg of IReactProperty * seq<ReactElement>
    | DefaultWithButton of seq<ReactElement> * string * 't
    | DefaultTemporary of IReactProperty * seq<ReactElement> * int

type PopUpControl =
    {
        Style : IReactProperty
        Button : ReactElement option
        Messages : seq<ReactElement>
    }

type Msg =
    | LoginAttemptMsg of string * string
    | LoadedInstructions of AsyncOperationEvent<Result<UserData, string>>
    | LoadedUsers of AsyncOperationEvent<Result<LoginInfo, string>>
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
    | NewInstructionToSave of Data.InstructionData * string
    | PopUpMsg of PopUpSettings<Msg -> unit> Option
type Model =
    {
      User : Data.Deferred<Result<LoginInfo, string>>
      UserFromLogin : ValidationLoginInfo
      LoginMessage : string
      Id : int
      CurrentPage: Global.UserPage
      InstructionSearch: InstructionSearch.Types.Model
      NewAdd : NewAdd.Types.Model
      UserData : Data.Deferred<Result<UserData, string>>
      Instruction: Instruction.Types.Model
      LoginSpinner : AppearanceAttributes
      PopUp : PopUpControl Option
    }
