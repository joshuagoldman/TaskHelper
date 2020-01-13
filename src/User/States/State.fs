module User.State

open Elmish
open Elmish.Navigation
open Fable.SimpleHttp
open Browser
open Global
open Data
open Types
open Fable.React
open Browser
open Thoth.Json

let urlUpdate (result : UserPage option) model =
    match result with
    | None ->
        console.error("Error parsing url")
        model, Navigation.modifyUrl (toHashUser model.CurrentPage)
    | Some page ->
        { model with CurrentPage = page }, []

let init() : Model * Cmd<Msg> =
    {
        CurrUser = HasNostStartedYet
        Id = 0
        LoginMessage = "Wilkommen"
        UserFromLogin =
            {
                Username = Invalid
                Password = Invalid
            }
        CurrentPage = InstructionSearch
        InstructionSearch = InstructionSearch.State.init() |> fun (a,_) -> a
        Category = Category.State.init() |> fun (a,_) -> a
        UserData = Data.HasNostStartedYet
        NewAdd = NewAdd.State.init() |> fun(a,_) -> a
        Instruction = Instruction.State.init() |> fun (a,b) -> a
    }, []

let update msg model : Model * Cmd<User.Types.Msg> =
    match msg with
    | LoadedInstructions Started ->
        { model with UserData = InProgress }, Cmd.batch
                                                [   Cmd.ofMsg (Logic.getUserDataUpdate InProgress)
                                                    Cmd.fromAsync User.Logic.loadInstructionItems
                                                  ]
    | LoadedInstructions (Finished (Error error)) ->
        { model with UserData = Resolved ( Error error)}, Cmd.ofMsg (Logic.getUserDataUpdate
                                                                            (Resolved ( Error error)))
    | LoadedInstructions (Finished (Ok items)) ->
        { model with UserData = Resolved ( Ok items)}, Cmd.ofMsg (Logic.getUserDataUpdate
                                                                            (Resolved ( Ok items)))
    | LoadedUsers Started ->
        { model with CurrUser = InProgress } , Cmd.batch
                                                [ Cmd.ofMsg (Logic.loginAttempt model InProgress)
                                                  Cmd.fromAsync User.Logic.loadUserItems
                                                  ]
                                                        
    | LoadedUsers (Finished (Error error)) ->
        { model with CurrUser = Resolved ( Error error)}, Cmd.ofMsg (Logic.loginAttempt model
                                                                                        (Resolved ( Error error)))
    | LoadedUsers (Finished (Ok items)) ->
        { model with CurrUser = Resolved ( Ok items)}, Cmd.ofMsg (Logic.loginAttempt model
                                                                                     (Resolved ( Ok items)))
    | InstructionMsg msg ->
        let (instruction, instructionCmd) = Instruction.State.update msg model.Instruction
        { model with Instruction = instruction}, Cmd.map InstructionMsg instructionCmd
    | InstructionSearchMsg msg ->
        let (instructionSearch, instructionSearchCmd) = InstructionSearch.State.update msg model.InstructionSearch
        { model with InstructionSearch = instructionSearch }, Cmd.map InstructionSearchMsg instructionSearchCmd
    | CategorySearchMsg msg ->
        let (category, categoryCmd) = Category.State.update msg model.Category
        { model with Category = category }, Cmd.map CategorySearchMsg categoryCmd
    | NewAddMsg msg ->
        let (newAdd, newAddCmd) = NewAdd.State.update msg model.NewAdd
        { model with NewAdd = newAdd }, Cmd.map NewAddMsg newAddCmd
    | LoginTimedOutMsg ->
        { model with CurrUser = HasNostStartedYet }, []
    | LoginMessages msg ->
        { model with LoginMessage = msg}, []
    | UserNameInputChangedMsg usrName -> { model with UserFromLogin =
                                                        { model.UserFromLogin with Username = Valid usrName } }, []
    | PasswordInputChangedMsg passwrd ->  { model with UserFromLogin =
                                                        { model.UserFromLogin with Password = Valid passwrd } }, []
