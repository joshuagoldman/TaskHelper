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
open Logic
open Feliz

let urlUpdate (result : UserPage option) model =
    match result with
    | None ->
        console.error("Error parsing url")
        model, Navigation.modifyUrl (toHashUser model.CurrentPage)
    | Some page ->
        { model with CurrentPage = page }, []

let init() : Model * Cmd<Msg> =
    {
        AllUsers = HasNostStartedYet
        Id = 0
        LoginMessage = "Please log on!"
        UserFromLogin =
            {
                Username = Invalid
                Password = Invalid
            }
        CurrentPage = InstructionSearch
        InstructionSearch = InstructionSearch.State.init() |> fun (a,_) -> a
        UserData = Data.HasNostStartedYet
        NewAdd = NewAdd.State.init() |> fun(a,_) -> a
        Instruction = Instruction.State.init() |> fun (a,b) -> a
        LoginSpinner =
            { Controls.defaultAppearanceAttributes with Visible = style.visibility.hidden }
    }, []

let matchValidity validityObject =
    match validityObject with
    | Valid str -> str
    | Invalid -> "Invalid"

let update msg model : Model * Cmd<User.Types.Msg> =
    match msg with
    | LoadedInstructions Started ->
        console.log("LoadedInstructions Started")
        { model with UserData = InProgress }, Cmd.batch
                                                    (Logic.getUserDataUpdate InProgress
                                                    |> Seq.map (fun msg -> Cmd.ofMsg msg)
                                                    |> Seq.append [Cmd.fromAsync (Logic.loadInstructionItems model)])
                                                    
    | LoadedInstructions (Finished (Error error)) ->
        { model with UserData = Resolved ( Error error)}, Cmd.batch
                                                                (Logic.getUserDataUpdate
                                                                            (Resolved ( Error error))
                                                                |> Seq.map (fun msg -> Cmd.ofMsg msg)
                                                                |> Seq.append(
                                                                    style.visibility.hidden
                                                                    |> ( LoginSpinnerMsg >> Cmd.ofMsg)
                                                                    |> fun x -> seq[x]
                                                                   ))
    | LoadedInstructions (Finished (Ok items)) ->
        { model with UserData = Resolved (Ok items) }, Cmd.batch
                                                            (Logic.getUserDataUpdate (Resolved ( Ok items))
                                                            |> Seq.map (fun msg -> Cmd.ofMsg msg)
                                                            |> Seq.append(
                                                                seq[
                                                                    (style.visibility.hidden
                                                                    |> ( LoginSpinnerMsg >> Cmd.ofMsg))
                                                                    (items |> (sleepAndLogin >> Cmd.fromAsync))
                                                                ]
                                                            )
                                                            |> Seq.append [ NewAdd.Logic.createNewInstructionSequence items |> Cmd.ofMsg])
                                                        
                                                            
                                                        
    | LoadedUsers Started ->
        { model with AllUsers = InProgress } , Cmd.batch
                                                    (Logic.loginAttempt model InProgress
                                                    |> Seq.map (fun msg -> Cmd.ofMsg msg )
                                                    |> Seq.append [Cmd.fromAsync Logic.loadUserItems])
                                                  
                                                        
    | LoadedUsers (Finished (Error error)) ->
        { model with AllUsers = Resolved ( Error error)}, Cmd.batch
                                                                (Logic.loginAttempt model (Resolved ( Error error))
                                                                |> Seq.map (fun msg -> Cmd.ofMsg msg ))
    | LoadedUsers (Finished (Ok items)) ->
        { model with AllUsers = Resolved ( Ok items)}, Cmd.batch
                                                            (Logic.loginAttempt model (Resolved ( Ok items))
                                                            |> Seq.map (fun msg -> Cmd.ofMsg msg ))
                                                                                    
    | InstructionMsg msg ->
        let (instruction, instructionCmd) = Instruction.State.update msg model.Instruction
        { model with Instruction = instruction}, instructionCmd
    | InstructionSearchMsg msg ->
        let (instructionSearch, instructionSearchCmd) = InstructionSearch.State.update msg model.InstructionSearch
        { model with InstructionSearch = instructionSearch }, Cmd.map InstructionSearchMsg instructionSearchCmd
    | NewAddMsg msg ->
        let (newAdd, newAddCmd) = NewAdd.State.update msg model.NewAdd
        { model with NewAdd = newAdd }, newAddCmd
    | LoginMessages msg ->
        { model with LoginMessage = msg}, []
    | UserNameInputChangedMsg usrName ->
        let newModel = 
            { model with UserFromLogin =
                          { model.UserFromLogin with Username =
                                                      validateLoginInfo usrName } }, []
        newModel
    | PasswordInputChangedMsg passwrd ->
        let newModel = 
            { model with UserFromLogin =
                          { model.UserFromLogin with Password =
                                                      validateLoginInfo passwrd } }, []
        newModel
    | LoginSuceeded data ->
        { model with CurrentPage = UserPage.InstructionSearch ;
                     UserData = Resolved(Ok data)}, []
    | NewUserId id ->
        { model with Id = id }, []

    | LoginSpinnerMsg visibility ->
        { model with LoginSpinner =
                        { model.LoginSpinner with Visible = visibility } }, []

    | NewUserDataToAddMsg instructionToAdd ->
        match model.UserData with
        | Data.Deferred.Resolved(Ok data) ->
            let newInstructions =
                data.Instructions
                |> Seq.append [instructionToAdd]
            {
                Id = data.Id
                Instructions = newInstructions

            }
            |> ( Ok >> Deferred.Resolved )
            |> fun newUserData ->
                { model with UserData = newUserData}, Cmd.none
                
        | Data.Deferred.Resolved(Error err) ->
            seq[
                divWithStyle
                    None
                    (err)
                    (prop.style[style.color.black ; style.fontWeight.bolder])
            ]
            |> ( NewAdd.Types.NewAddInfoMsg >> User.Types.NewAddMsg)
            |> Cmd.ofMsg
            |> fun msg -> model, msg

        | _ -> model, Cmd.none
    | ChangePage info ->
        match info with
        | Delay (page,delay) ->
            model, page
                   |> (NoDelay >> User.Types.ChangePage)
                   |> Logic.delayedMessage delay
                   |> Cmd.fromAsync

        | NoDelay page ->
            { model with CurrentPage = page}, Cmd.none
    | NewAddNewCurrentInstruction titleOpt ->
            let usrId = model.Id |> string
            match model.UserData with
            | Resolved (Ok data) ->
                let dataIsNew =
                    data.Instructions
                    |> Seq.length
                    |> function pos ->
                        (None,pos |> string |> fun x -> usrId + "_" + x) |>
                        ( NewAdd.Types.NewCurrentInstructionMsg >>
                          User.Types.NewAddMsg >> Cmd.ofMsg )

                titleOpt
                |> function
                    | res when res.IsSome ->
                        Seq.zip data.Instructions ( Global.getPositionSequence data.Instructions )
                        |> Seq.tryFind (fun (instruction,_) -> instruction.Title = titleOpt.Value)
                        |> function
                            | res when res.IsSome ->
                                res.Value
                                |> fun (instr,pos) ->
                                    (Some instr,pos |> string |> fun x -> usrId + "_" + x) |>
                                    ( NewAdd.Types.NewCurrentInstructionMsg >>
                                      User.Types.NewAddMsg >> Cmd.ofMsg )
                            | _ -> dataIsNew
                    | _ -> dataIsNew
                |> fun msg ->
                    model, msg
            | _ -> model, []

    | GiveResetInstruction str ->
        let msg =
            match model.UserData with
            | Resolved (Ok data) ->
                data.Instructions
                |> Seq.tryFind (fun instruction ->
                    instruction.Title = str)
                |> function
                    | res when res.IsSome ->
                        res.Value
                        |> (Instruction.Types.ResetActions.ResetInstructionObtained >>
                            Instruction.Types.Reset >>
                            User.Types.InstructionMsg >>
                            Cmd.ofMsg) 
                    | _ -> []
            | _ -> []
        model, msg
