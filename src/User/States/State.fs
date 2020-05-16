module User.State

open Elmish
open Elmish.Navigation
open Fable.SimpleHttp
open Browser
open Global
open Data
open Types
open Fable.React
open Fable.React.Helpers
open Browser
open Thoth.Json
open Logic
open Feliz
open Elmish.React

let urlUpdate (result : UserPage option) model =
    match result with
    | None ->
        console.error("Error parsing url")
        model, Navigation.modifyUrl (toHashUser model.CurrentPage)
    | Some page ->
        { model with CurrentPage = page }, []

let init() : Model * Cmd<Msg> =
    {
        User = HasNostStartedYet
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
        PopUp = None
    }, []

let matchValidity validityObject =
    match validityObject with
    | Valid str -> str
    | Invalid -> "Invalid"

let update msg model : Model * Cmd<User.Types.Msg> =
    match msg with
    | MsgNone ->
        model,Cmd.none
    | CmdMsging cmdMsg ->
        model,cmdMsg
    | LoadedInstructions Started ->
        console.log("LoadedInstructions Started")
        { model with UserData = InProgress }, Cmd.batch
                                                    (Logic.getUserDataUpdate InProgress
                                                    |> Array.map (fun msg -> Cmd.ofMsg msg)
                                                    |> Array.append [|Cmd.fromAsync (Logic.loadInstructionItems model.Id)|])
                                                    
    | LoadedInstructions (Finished (Error error)) ->
        { model with UserData = Resolved ( Error error)}, Cmd.batch
                                                                (Logic.getUserDataUpdate
                                                                            (Resolved ( Error error))
                                                                |> Array.map (fun msg -> Cmd.ofMsg msg)
                                                                |> Array.append(
                                                                    style.visibility.hidden
                                                                    |> ( LoginSpinnerMsg >> Cmd.ofMsg)
                                                                    |> fun x -> [|x|]
                                                                   ))
    | LoadedInstructions (Finished (Ok items)) ->
        { model with UserData = Resolved (Ok items) }, Cmd.batch
                                                            (Logic.getUserDataUpdate (Resolved ( Ok items))
                                                            |> Array.map (fun msg -> Cmd.ofMsg msg)
                                                            |> Array.append(
                                                                [|
                                                                    (style.visibility.hidden
                                                                    |> ( LoginSpinnerMsg >> Cmd.ofMsg))
                                                                    (items
                                                                     |> LoginSuceeded
                                                                     |> delayedMessage 2000
                                                                     |> Cmd.fromAsync)
                                                                |]
                                                            )
                                                            |> Array.append (NewAdd.Logic.createNewInstructionSequence items)
                                                    )                                        
    | LoadedUsers Started ->
        let (username,password) =
            getLoginInfo model

        let userValidationMsg =
            getUserValidationMsg username password
                    
        { model with User = InProgress } , Cmd.batch
                                                    (Logic.loginAttempt model InProgress
                                                    |> Array.map (fun msg -> Cmd.ofMsg msg )
                                                    |> Array.append [|userValidationMsg|])
                                                  
                                                        
    | LoadedUsers (Finished (Error error)) ->
        { model with User = Resolved ( Error error)}, Cmd.batch
                                                                (Logic.loginAttempt model (Resolved ( Error error))
                                                                |> Array.map (fun msg -> Cmd.ofMsg msg ))
    | LoadedUsers (Finished (Ok items)) ->
        { model with User = Resolved ( Ok items)}, Cmd.batch
                                                            (Logic.loginAttempt model (Resolved ( Ok items))
                                                            |> Array.map (fun msg -> Cmd.ofMsg msg ))
                                                                                    
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
        let msg =
            Elmish.Navigation.Navigation.newUrl(Global.toHash(User(InstructionSearch)))
        { model with CurrentPage = UserPage.InstructionSearch ;
                     UserData = Resolved(Ok data)}, msg
    | NewUserId id ->
        { model with Id = id }, []

    | LoginSpinnerMsg visibility ->
        { model with LoginSpinner =
                        { model.LoginSpinner with Visible = visibility } }, []

    | NewUserDataToAddMsg instructionToAdd ->
        match model.UserData with
        | Data.Deferred.Resolved(Ok data) ->
            let newInstructionWInfo =
                data.Instructions
                |> Array.map (fun instructionComp ->
                    instructionComp.Title.Replace(" ","") = instructionToAdd.Title.Replace(" ","")
                    |>function
                        | areEq when areEq = true ->
                            (true,instructionToAdd)
                        | _ ->
                            (false,instructionComp))
            newInstructionWInfo
            |> Array.exists (fun (isInstrWNewInfo,_) -> isInstrWNewInfo)
            |> function
                | existsDuplicateInstructionTitle when existsDuplicateInstructionTitle ->
                    let newInstr =
                        newInstructionWInfo
                        |> Array.map (fun (_,instr) -> instr)

                    {
                        Id = data.Id
                        Instructions = newInstr

                    }
                    |> ( Ok >> Deferred.Resolved )
                    |> fun newUserData ->
                        { model with UserData = newUserData}, Cmd.none
                | _ ->
                    let newInstructions =
                        data.Instructions
                        |> Array.append [|instructionToAdd|]
                    
                    {
                        Id = data.Id
                        Instructions = newInstructions

                    }
                    |> ( Ok >> Deferred.Resolved )
                    |> fun newUserData ->
                        { model with UserData = newUserData}, Cmd.none
                
        | Data.Deferred.Resolved(Error err) ->
            [|
                divWithStyle
                    None
                    (err)
                    (prop.style[style.color.black ; style.fontWeight.bolder])
            |]
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
            let msg =
                Elmish.Navigation.Navigation.newUrl(Global.toHash(User(page)))
            { model with CurrentPage = page}, msg

    | NewAddNewCurrentInstruction titleOpt ->
            let usrId = model.Id |> string
            match model.UserData with
            | Resolved (Ok data) ->
                let dataIsNew =
                    (None, usrId)
                    |>(Some >>
                       NewAdd.Types.NewCurrentInstructionMsg >>
                       User.Types.NewAddMsg >> Cmd.ofMsg )
                titleOpt
                |> function
                    | res when res.IsSome ->
                        data.Instructions
                        |> Array.tryFind (fun (instruction) ->
                            instruction.Title.Trim() = titleOpt.Value.Trim())
                        |> function
                            | res when res.IsSome ->
                                res.Value
                                |> fun instr ->
                                    (Some instr,usrId) |>
                                    ( Some >>
                                      NewAdd.Types.NewCurrentInstructionMsg >>
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
                Array.zip data.Instructions [|0..data.Instructions |> Array.length |> fun x -> x - 1|]
                |> Array.tryFind (fun (instruction,pos) ->
                    instruction.Title = str)
                |> function
                    | res when res.IsSome ->
                        let (instr,id) = res.Value |> fun  (a,b) -> (a,b |> string)
                        (instr,id)
                        |> (Instruction.Types.NewInstruction2Show >>
                            User.Types.InstructionMsg >>
                            Cmd.ofMsg) 
                    | _ -> []
            | _ -> []
        model, msg

    | PopUpMsg settings ->
        let standardResult =
            { model with PopUp = None},[]

        match settings with
        | Some settings ->
            let popupOpt =
                Logic.getPopupWindow settings
            
            match popupOpt with
            | Some (popup,msg) ->
                { model with PopUp = Some popup}, msg
            | _ -> standardResult

        | None ->
            standardResult
    | CompareNewSaveWithCurrentInstructions (instruction,instructionInfo,positions) ->
        let msg =
            Logic.savingChoices
                        model.UserData
                        positions
                        instruction
                        instructionInfo

        model,msg
    | DeleteInstructionMsg(delInstruction,positions) ->
        match model.UserData with
        | Resolved(Ok(usrData)) ->
            usrData.Instructions
            |> Array.indexed
            |> Array.tryFind (fun (_,instr) ->
                instr.Title.Replace(" ","") = delInstruction.Title.Replace(" ",""))
            |> function
                | delInstrWithPos when delInstrWithPos.IsSome ->
                    let (instrId,_) = delInstrWithPos.Value
                    let usrId = usrData.Id

                    let dbIds =
                        {
                            UserId = usrId |> string
                            InstructionId = instrId |> string
                        }

                    let startDeletionMsg =
                        delInstruction
                        |> Data.DatabaseDeleteOptions.DeleteInstruction
                        |> Data.DatabaseSavingOptions.PartsToDeleteInstruction
                        |> fun x -> [|x|]
                        |> fun saveOpt ->
                            (saveOpt,dbIds,positions)
                            |>Instruction.Types.DatabaseChangeBegun
                            |> Instruction.Types.Msg.DatabaseChangeMsg
                            |> User.Types.InstructionMsg
                            |> Cmd.ofMsg

                    model, startDeletionMsg
                | _ -> model,[]
        | _ -> model,[]
        
