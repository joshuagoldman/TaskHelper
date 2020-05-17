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
    | DefaultWithButton of array<ReactElement> * Position
    | Default of array<ReactElement> * Position
    | DefaultWithOptions of array<ReactElement> * Position * array<'a>
    | OptionalWithMsg of array<ReactElement> * Position * array<IStyleAttribute>
    | DefaultNewPage of array<ReactElement> * NewUserPage * Position

// Many discriminated unions in order to facilitate unit testing
type newSaveResult =
    | SaveNew of Data.InstructionData * string
    | SaveExistingNewTitles of array<DatabaseSavingOptions> * string
    | SaveExisitngNewFIles of array<DatabaseSavingOptions> * string
    | SaveExistingNewFilesAndTItles of array<DatabaseSavingOptions> * string
    | SaveExistingNewFilesPartsToDelete of array<DatabaseSavingOptions> * string
    | SaveExistingNewTItlesPartsToDelete of array<DatabaseSavingOptions> * string
    | SaveExistingNewFilesAndTItlesPartsToDelete of array<DatabaseSavingOptions> * string
    | SaveExistingPartsToDelete of array<DatabaseSavingOptions> * string
    | InstructionIsDelete of string
    | NoUserData of string
    | ThatInstructionAlreadyExists of string
    | InstructionHasNotDistinctTitles of string

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
    | NewUserDataInstructionToPossiblyAdd of UpdateUserInstructionsType
    | NewUserDataToAddMsg
    | ChangePage of NewUserPage
    | NewAddNewCurrentInstruction of Option<string>
    | GiveResetInstruction of string
    | NewInstructionToSave of Data.InstructionData * string
    | PopUpMsg of PopUpSettings<Msg> Option
    | CompareNewSaveWithCurrentInstructions of Data.InstructionData *
                                               Option<array<Instruction.Types.modificationInfo>> *
                                               Position
    | SaveInstructionToDataBase of Data.InstructionData * string
    | DeleteInstructionMsg of Data.InstructionData * Position
    | CmdMsging of Elmish.Cmd<Msg>
    | MsgNone
    | GetIdsForNewInstrUpload of array<NewAdd.Types.MediaChoiceFormData> * InstructionData option

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
      PossibleNewInstruction: NewPossibleInstructionOptions
      Instruction: Instruction.Types.Model
      LoginSpinner : AppearanceAttributes
      PopUp : PopUpControl<Msg> Option
    }
