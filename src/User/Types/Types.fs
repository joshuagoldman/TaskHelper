module User.Types

open Global
open Data
open Controls
open Feliz
open Browser


type NewUserPage =
    | NoDelay of UserPage
    | Delay of UserPage * int

type UpdateUserInstructionsType =
    | AddNewInstruction of InstructionData
    | DeleteInstruction of InstructionData
    | UpdateInstruction of InstructionData

type NewPossibleInstructionOptions =
    | NoSaveOrDeleteAttempt
    | SaveOrDeleteAttempt of UpdateUserInstructionsType

type PopUpControl<'a> =
    {
        Style : IReactProperty
        ButtonSettings : option<array<IStyleAttribute>>
        ClickMessages : option<array<'a>>
        Messages : array<ReactElement>
    }

type PopUpSettings<'a> =
    | DefaultWithButton of array<ReactElement> * Utilities<'a>
    | Default of array<ReactElement> * Utilities<'a>
    | DefaultWithOptions of array<ReactElement> * Utilities<'a> * array<'a>
    | OptionalWithMsg of array<ReactElement> * Utilities<'a> * array<IStyleAttribute>
    | DefaultNewPage of array<ReactElement> * NewUserPage * Utilities<'a>

// Many discriminated unions in order to facilitate unit testing
type newSaveResult =
    | SaveNew of Data.InstructionData
    | SaveExistingNewTitles of array<DatabaseSavingOptions>
    | SaveExisitngNewFIles of array<DatabaseSavingOptions>
    | SaveExistingNewFilesAndTItles of array<DatabaseSavingOptions>
    | SaveExistingNewFilesPartsToDelete of array<DatabaseSavingOptions>
    | SaveExistingNewTItlesPartsToDelete of array<DatabaseSavingOptions>
    | SaveExistingNewFilesAndTItlesPartsToDelete of array<DatabaseSavingOptions>
    | SaveExistingPartsToDelete of array<DatabaseSavingOptions>
    | InstructionIsDelete of string
    | NoUserData of string
    | ThatInstructionAlreadyExists of string
    | InstructionHasNotDistinctTitles of string

type Msg =
    | LoadedInstructions of AsyncOperationEvent<Result<UserData, string>>
    | LoadedUsers of AsyncOperationEventWithDispatch<Msg -> unit,Result<LoginInfo * (Msg -> unit), string>>
    | InstructionMsg of Instruction.Types.Msg<Msg>
    | InstructionSearchMsg of InstructionSearch.Types.Msg<Msg>
    | NewAddMsg of NewAdd.Types.Msg<Msg>
    | LoginMessages of string
    | UserNameInputChangedMsg of string
    | PasswordInputChangedMsg of string
    | LoginSuceeded of UserData
    | NewUserId of int
    | LoginSpinnerMsg of IStyleAttribute
    | PossibleNewUserDataMsg of UpdateUserInstructionsType
    | NewUserDataToAddMsg
    | ChangePage of NewUserPage
    | NewAddNewCurrentInstruction of Option<string>
    | GiveResetInstruction of string
    | PopUpMsg of PopUpSettings<Msg> Option
    | CompareNewSaveWithCurrentInstructions of Data.InstructionData *
                                               Option<array<Instruction.Types.modificationInfo>> *
                                               Utilities<Msg>
    | DeleteInstructionMsg of Data.InstructionData * Utilities<Msg>
    | CmdMsging of Elmish.Cmd<Msg>
    | MsgNone
    | GetIdsForNewInstrUpload of array<NewAdd.Types.MediaChoiceFormData> * InstructionData option
    | SaveNewData of DatabaseNewFilesOptions * Utilities<Msg> * NewAdd.Types.MediaChoiceFormData []
    | GetUserDispatchMsg of UsrTypeDispatchOptions<Msg>

type Model =
    {
      User : Data.Deferred<Result<LoginInfo, string>>
      UserFromLogin : ValidationLoginInfo
      LoginMessage : string
      Id : int
      CurrentPage: Global.UserPage
      InstructionSearch: InstructionSearch.Types.Model<Msg>
      NewAdd : NewAdd.Types.Model
      UserData : Data.Deferred<Result<UserData, string>>
      PossibleNewInstruction: NewPossibleInstructionOptions
      Instruction: Instruction.Types.Model<Msg>
      LoginSpinner : AppearanceAttributes
      PopUp : PopUpControl<Msg> Option
      Dispatch : UsrTypeDispatchOptions<Msg>
    }
