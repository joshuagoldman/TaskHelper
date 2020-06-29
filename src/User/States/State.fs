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
        UserData = Data.Deferred.HasNostStartedYet
        NewAdd = NewAdd.State.init() |> fun(a,_) -> a
        PossibleNewInstruction = NoSaveOrDeleteAttempt
        Instruction = Instruction.State.init() |> fun (a,b) -> a
        LoginSpinner =
            { Controls.defaultAppearanceAttributes with Visible = style.visibility.hidden }
        PopUp = None
        Dispatch = NoDispatchDefined
    }, []

let matchValidity validityObject =
    match validityObject with
    | Valid str -> str
    | Invalid -> "Invalid"

let update msg model : Model * Cmd<User.Types.Msg> =
    match msg with
    | GetUserDispatchMsg dispatchOpt ->
        match dispatchOpt with
        | DispatchDefined dispatch ->
            let instrMsg =
                { model.Instruction with UserTypeDispatch = dispatch |> DispatchDefined }

            { model with Dispatch = dispatch |> DispatchDefined ;
                         Instruction = instrMsg},[]
        | _ -> model,[]
    | MsgNone ->
        model,Cmd.none
    | CmdMsging cmdMsg ->
        model,cmdMsg
    | LoadedInstructions AsyncOperationEvent.Started ->
        console.log("LoadedInstructions Started")
        { model with UserData = InProgress }, Cmd.batch
                                                    (Logic.getUserDataUpdate InProgress
                                                    |> Array.map (fun msg -> Cmd.ofMsg msg)
                                                    |> Array.append [|Cmd.fromAsync (Logic.loadInstructionItems model.Id)|])
                                                    
    | LoadedInstructions (AsyncOperationEvent.Finished (Error error)) ->
        { model with UserData = Resolved ( Error error)}, Cmd.batch
                                                                (Logic.getUserDataUpdate
                                                                            (Resolved ( Error error))
                                                                |> Array.map (fun msg -> Cmd.ofMsg msg)
                                                                |> Array.append(
                                                                    style.visibility.hidden
                                                                    |> ( LoginSpinnerMsg >> Cmd.ofMsg)
                                                                    |> fun x -> [|x|]
                                                                   ))
    | LoadedInstructions (AsyncOperationEvent.Finished (Ok items)) ->
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
    | LoadedUsers(AsyncOperationEventWithDispatch.Started dispatch) ->
        let (username,password) =
            getLoginInfo model

        let userValidationMsg =
            getUserValidationMsg username password dispatch
                    
        { model with User = InProgress } , Cmd.batch
                                                    (Logic.loginAttempt ( dispatch |> DeferredWithDispatch.InProgress )
                                                    |> Array.map (fun msg -> Cmd.ofMsg msg )
                                                    |> Array.append [|userValidationMsg|])
                                                  
                                                        
    | LoadedUsers (Finished (Error error)) ->
        { model with User = Resolved ( Error error)}, Cmd.batch
                                                                (Logic.loginAttempt (DeferredWithDispatch.Resolved ( Error error))
                                                                |> Array.map (fun msg -> Cmd.ofMsg msg ))
    | LoadedUsers (Finished (Ok (items,dispach))) ->
        { model with User = Deferred.Resolved ( Ok items)}, Cmd.batch
                                                            (Logic.loginAttempt (DeferredWithDispatch.Resolved ( Ok (items,dispach)))
                                                            |> Array.map (fun msg -> Cmd.ofMsg msg ))
                                                                                    
    | InstructionMsg msg ->
        let (instruction, instructionCmd) = Instruction.State.update msg model.Instruction
        { model with Instruction = instruction}, instructionCmd
    | InstructionSearchMsg msg ->
        let (instructionSearch, instructionSearchCmd) = InstructionSearch.State.update msg model.InstructionSearch
        { model with InstructionSearch = instructionSearch }, instructionSearchCmd
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

    | PossibleNewUserDataMsg updateType ->
        { model with PossibleNewInstruction = NewPossibleInstructionOptions.SaveOrDeleteAttempt(updateType)}, []

    | NewUserDataToAddMsg ->
        match model.UserData with
        | Data.Deferred.Resolved(Ok data) ->
            let result =
                Instruction.Logic.updateUserInstructions model.PossibleNewInstruction data.Instructions data.Id

            match result with
            | Some newUserData ->
                match newUserData with
                | Deferred.Resolved(Ok usrData) ->

                    let addInstructionListToNewAddMsg =
                        usrData.Instructions
                        |> Array.map (fun instr ->
                            match instr.Title with
                            | InstructionTitleInfo.HasOldName title ->
                                title
                            | InstructionTitleInfo.HasNewName titles ->
                                titles.OldName)|>
                        (
                            NewAdd.Types.NewInstructionsListMsg >>
                            User.Types.NewAddMsg >>
                            Cmd.ofMsg
                        )

                    let msgCommand =
                        usrData.Instructions
                        |> Array.indexed
                        |> Array.head
                        |> fun (instrId,instr) ->

                            let escapeModifyModeCommandMsg =
                                style.visibility.hidden |>
                                Instruction.Types.ModifyInstructionMsg
                                |> User.Types.InstructionMsg
                                |> Cmd.ofMsg
                                |> fun msg1 ->
                                        [|
                                            msg1
                                            (true |>
                                             Instruction.Types.DeleteButtonEnablenMsg)
                                             |> User.Types.InstructionMsg
                                             |> Cmd.ofMsg
                                        |]
                                |> Cmd.batch

                            let instructionSearchCommand =
                                let parthRes =
                                    instr.Data
                                    |> Array.map (fun part ->
                                        (part,Cmd.none,instr,Cmd.none)
                                        |> InstructionSearch.Types.Part
                                        |> Ok)

                                let instrRes =
                                    (instr,instrId |> string,Cmd.none) |> InstructionSearch.Types.Instruction
                                    |> Ok
                                    |> fun x -> [|x|]

                                let searchRes =
                                    parthRes
                                    |> Array.append instrRes

                                searchRes
                                |> InstructionSearch.Types.GetNewInstruction
                                |> User.Types.InstructionSearchMsg
                                |> Cmd.ofMsg

                            let instructionMsgCommand =
                                (instr,instrId |> string)
                                |> Instruction.Types.NewInstruction2Show
                                |> InstructionMsg
                                |> Cmd.ofMsg

                            [|
                                instructionSearchCommand
                                instructionMsgCommand
                                escapeModifyModeCommandMsg
                                addInstructionListToNewAddMsg
                            |]
                            |> Cmd.batch

                    { model with UserData = newUserData}, msgCommand
                | _ -> model,[]
            | _ -> model, []
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

                let resultMsg =
                    titleOpt
                    |> function
                        | res when res.IsSome ->
                            data.Instructions
                            |> Array.tryFind (fun instruction ->
                                match instruction.Title with
                                | Data.InstructionTitleInfo.HasOldName title ->
                                    title.Replace(" ","") = titleOpt.Value.Replace(" ","")
                                | Data.InstructionTitleInfo.HasNewName titles ->
                                    titles.DbName.Replace(" ","") = titleOpt.Value.Replace(" ",""))
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
                resultMsg
                |> fun msg ->
                    model, msg
            | _ -> model, []

    | GiveResetInstruction str ->
        let msg =
            match model.UserData with
            | Resolved (Ok data) ->
                Array.zip data.Instructions [|0..data.Instructions |> Array.length |> fun x -> x - 1|]
                |> Array.tryFind (fun (instruction,pos) ->
                    match instruction.Title with
                    | Data.InstructionTitleInfo.HasOldName title ->
                        title = str
                    | Data.InstructionTitleInfo.HasNewName titles ->
                        titles.OldName = str)
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
    | CompareNewSaveWithCurrentInstructions (instruction,instructionInfo,utils) ->
        let msg =
            Logic.savingChoices
                        model.UserData
                        utils
                        instruction
                        instructionInfo

        model,msg
    | DeleteInstructionMsg(delInstruction,utils) ->
        match model.UserData with
        | Resolved(Ok(usrData)) ->
            let foundExistingInstruction = 
                usrData.Instructions
                |> Array.indexed
                |> Array.tryFind (fun (_,existingInstr) ->
                    match existingInstr.Title with
                    | Data.InstructionTitleInfo.HasOldName titleFromDataBase ->
                        match delInstruction.Title with
                        | Data.InstructionTitleInfo.HasOldName delTitle ->
                            titleFromDataBase.Replace(" ","") = delTitle.Replace(" ","")
                        | Data.InstructionTitleInfo.HasNewName titles ->
                            titleFromDataBase.Replace(" ","") = titles.DbName.Replace(" ","")
                    | Data.InstructionTitleInfo.HasNewName titles ->
                        match delInstruction.Title with
                        | Data.InstructionTitleInfo.HasOldName delTitle->
                            titles.DbName.Replace(" ","") = delTitle.Replace(" ","")
                        | Data.InstructionTitleInfo.HasNewName delTitles ->
                            titles.DbName.Replace(" ","") = delTitles.DbName.Replace(" ",""))

            foundExistingInstruction
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
                            (saveOpt,dbIds,utils)
                            |>Instruction.Types.DatabaseChangeBegun
                            |> Instruction.Types.Msg.DatabaseChangeMsg
                            |> User.Types.InstructionMsg
                            |> Cmd.ofMsg

                    model, startDeletionMsg
                | _ -> model,[]
        | _ -> model,[]

    | GetIdsForNewInstrUpload(medias,instructionOpt) ->
        match model.UserData with
        | Deferred.Resolved(Ok usrData) ->

            let fileWPossibleNewName =
                Logic.changeFileNameIfNotUnique usrData.Instructions medias
            let newInstructionOption =
                usrData.Instructions
                |> Array.indexed
                |> Array.last
                |> fun (indx,_) ->
                    let dbIds =
                        {
                            UserId = model.Id |> string
                            InstructionId = indx + 1 |> string
                        }

                    let commandMsg =
                        Logic.createInstructionFromFile fileWPossibleNewName None dbIds
                        |> Array.map (fun msg -> msg |> Cmd.ofMsg)
                        |> Cmd.batch

                    model, commandMsg
            match instructionOpt with
            | Some instruction ->
                usrData.Instructions
                |> Array.indexed
                |> Array.tryFind (fun (_,existingInstr) ->
                    match existingInstr.Title with
                    | Data.InstructionTitleInfo.HasOldName titleFromDataBase ->
                        match instruction.Title with
                        | Data.InstructionTitleInfo.HasOldName delTitle ->
                            titleFromDataBase.Replace(" ","") = delTitle.Replace(" ","")
                        | Data.InstructionTitleInfo.HasNewName titles ->
                            titleFromDataBase.Replace(" ","") = titles.DbName.Replace(" ","")
                    | Data.InstructionTitleInfo.HasNewName titles ->
                        match instruction.Title with
                        | Data.InstructionTitleInfo.HasOldName delTitle->
                            titles.DbName.Replace(" ","") = delTitle.Replace(" ","")
                        | Data.InstructionTitleInfo.HasNewName delTitles ->
                            titles.DbName.Replace(" ","") = delTitles.DbName.Replace(" ",""))
                |> function
                    | res when res.IsSome ->
                        let (indx,_) = res.Value

                        let dbIds =
                            {
                                UserId = model.Id |> string
                                InstructionId = indx |> string
                            }

                        let commandMsg =
                            Logic.createInstructionFromFile fileWPossibleNewName (Some(instruction)) dbIds
                            |> Array.map (fun msg -> msg |> Cmd.ofMsg)
                            |> Cmd.batch

                        model, commandMsg
                    | _ ->
                        newInstructionOption
            | _ -> newInstructionOption
                        
        | _ -> model,[]
    | SaveNewData (dbNewFileOptions,dbIds,positions,medias) ->
        match model.UserData with
        | Resolved(Ok usrData) ->
            let mediaWPossibleNewNames =
                User.Logic.changeFileNameIfNotUnique usrData.Instructions medias
            let msg =
                Instruction.Logic.saveNewData mediaWPossibleNewNames
                                              dbNewFileOptions
                                              dbIds
                                              positions
            model,msg
        | _ -> model,[]

        
