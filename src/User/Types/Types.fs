module User.Types

open Global
open Data
open Controls
open Feliz
open Browser

type NewUserPage =
    | NoDelay of UserPage
    | Delay of UserPage * int

type RecursiveAction<'t,'u> =
    | First of 't
    | Second of 'u

type PopUpControl<'a> =
    {
        Style : IReactProperty
        ButtonSettings : option<seq<IStyleAttribute>>
        ClickMessages : option<seq<'a>>
        Messages : seq<ReactElement>
    }

type PopUpSettings<'a> =
    | DefaultWithButton of seq<ReactElement> * Position
    | DefaultWithOptions of seq<ReactElement> * Position * seq<'a>
    | OptionalWithMsg of seq<ReactElement> * Position * seq<IStyleAttribute>
    | DefaultNewPage of seq<ReactElement> * NewUserPage * Position

type DatabaseDeleteOptions =
    | DeleteInstruction of Data.InstructionData * DBIds
    | DeleteParts of seq<Data.partData> * string

type DatabaseSavingOptions = 
    | NewFilesInstruction of Data.InstructionData
    | NewNameInstruction of Data.InstructionData
    | PartsToDeleteInstruction of DatabaseDeleteOptions   

// Many discriminated union in order to facilitate unit testing
type newSaveResult =
    | SaveNew of Data.InstructionData * string
    | SaveExistingNewTitles of seq<DatabaseSavingOptions> * string
    | SaveExisitngNewFIles of seq<DatabaseSavingOptions> * string
    | SaveExistingNewFilesAndTItles of seq<DatabaseSavingOptions> * string
    | SaveExistingNewFilesPartsToDelete of seq<DatabaseSavingOptions> * string
    | SaveExistingNewTItlesPartsToDelete of seq<DatabaseSavingOptions> * string
    | SaveExistingNewFilesAndTItlesPartsToDelete of seq<DatabaseSavingOptions> * string
    | SaveExistingPartsToDelete of seq<DatabaseSavingOptions> * string
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
    | NewUserDataToAddMsg of Data.InstructionData
    | ChangePage of NewUserPage
    | NewAddNewCurrentInstruction of Option<string>
    | GiveResetInstruction of string
    | NewInstructionToSave of Data.InstructionData * string
    | PopUpMsg of PopUpSettings<Msg> Option
    | CompareNewSaveWithCurrentInstructions of Data.InstructionData *
                                               Option<seq<Instruction.Types.modificationInfo>> *
                                               Position
    | SaveInstructionToDataBase of Data.InstructionData * string

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
      PopUp : PopUpControl<Msg> Option
    }
