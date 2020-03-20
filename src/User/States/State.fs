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
    | LoadedInstructions Started ->
        console.log("LoadedInstructions Started")
        { model with UserData = InProgress }, Cmd.batch
                                                    (Logic.getUserDataUpdate InProgress
                                                    |> Seq.map (fun msg -> Cmd.ofMsg msg)
                                                    |> Seq.append [Cmd.fromAsync (Logic.loadInstructionItems model.Id)])
                                                    
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
                                                                    (items
                                                                     |> LoginSuceeded
                                                                     |> delayedMessage 2000
                                                                     |> Cmd.fromAsync)
                                                                ]
                                                            )
                                                            |> Seq.append (NewAdd.Logic.createNewInstructionSequence items)
                                                    )                                        
    | LoadedUsers Started ->
        let (username,password) =
            getLoginInfo model

        let userValidationMsg =
            getUserValidationMsg username password
                    
        { model with User = InProgress } , Cmd.batch
                                                    (Logic.loginAttempt model InProgress
                                                    |> Seq.map (fun msg -> Cmd.ofMsg msg )
                                                    |> Seq.append [userValidationMsg])
                                                  
                                                        
    | LoadedUsers (Finished (Error error)) ->
        { model with User = Resolved ( Error error)}, Cmd.batch
                                                                (Logic.loginAttempt model (Resolved ( Error error))
                                                                |> Seq.map (fun msg -> Cmd.ofMsg msg ))
    | LoadedUsers (Finished (Ok items)) ->
        { model with User = Resolved ( Ok items)}, Cmd.batch
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
                        |> Seq.tryFind (fun (instruction) ->
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
                Seq.zip data.Instructions [0..data.Instructions |> Seq.length |> fun x -> x - 1]
                |> Seq.tryFind (fun (instruction,pos) ->
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

    | NewInstructionToSave(instruction,instructionId) ->
        model,[]
        //let usrId = model.Id |> string
        //Logic.saveInstructionToDatabase (instruction,instructionId) usrId

    | PopUpMsg settings ->
        match settings with
        | Some settings ->
            match settings with
            | DefaultWithButton (str,dispatch,positions) ->
                let style =
                    prop.style[
                        style.zIndex 1
                        Feliz.style.left ( positions.X |> int )
                        Feliz.style.top ( positions.Y |> int )
                        style.position.absolute
                        style.backgroundColor.white
                        style.borderRadius 20
                        style.opacity 0.90
                    ]

                let button =
                    Html.div[
                        prop.className "columns is-centered"
                        prop.children[
                            Html.a[
                                prop.className "button"
                                prop.style[
                                    Feliz.style.margin 30
                                    Feliz.style.backgroundColor "grey"
                                    Feliz.style.fontSize 18
                                    Feliz.style.borderRadius 10
                                ]
                                prop.onClick (fun _ -> None |> ( PopUpMsg >> dispatch ) )
                                prop.children[
                                    Fable.React.Helpers.str "Ok"
                                ]
                            ]
                        ]
                    ]

                let popupSettings =
                    {
                        Style = style
                        Button = Some button
                        Messages = str
                    }
                    |> Some

                { model with PopUp = popupSettings},[]

            | OptionalWithMsg (divs,positions,styles) ->
                let style =
                    [
                        style.zIndex 1
                        Feliz.style.left ( positions.X |> int )
                        Feliz.style.top ( positions.Y |> int )
                        style.position.absolute
                        style.backgroundColor.white
                        style.borderRadius 20
                        style.opacity 0.85
                    ]
                    |> List.append (styles |> Seq.toList)
                    |> prop.style


                let popup =
                    {
                        Style = style
                        Button = None
                        Messages = divs
                    }
                    |> Some
                {model with PopUp = popup},[]
            | _ ->
                { model with PopUp = None},[]
        | None ->
            { model with PopUp = None},[]
