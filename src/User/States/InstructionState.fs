module Instruction.State

open Elmish
open Controls
open Types
open Part
open Data
open Feliz
open Fable.Core
open Browser

let init () : Model<'a> * Cmd<Msg<User.Types.Msg>> =

    {
      InstructionErrorMessage = defaultAppearanceAttributes
      CurrInstruction = Error ""
      CurrPart = Part.State.init() |> fun (a,b) -> a
      CurrPositions = None
      CurrTempInstruction = None
      PartNameModificationInput =
        { defaultAppearanceAttributes with Visible = style.visibility.hidden ; Disable = true }
      PositionsInput = defaultAppearanceAttributes
      DeleteButton =
        { defaultAppearanceAttributes with Disable = true }
      UserTypeDispatch = NoDispatchDefined
      FileAddMsg = defaultAppearanceAttributes
      CurrentDataBaseChanges = NoPendingDatabaseCHanges
    }, []


let update msg model : Instruction.Types.Model<User.Types.Msg> * Cmd<User.Types.Msg>  =
    match msg with
    | NewInstruction2Show (instruction,id) ->
        let delOrReg  =
            "Delete"
            |> ( Delete >> Some )
        let getStatus name =
            PartStatus.StatusExisting(name)
        let newModInfo =
            instruction.Data
            |> Array.map (fun part ->
                let names =
                    {
                        CurrName = part.Title
                        NewName = None
                    }
                {
                    DelOrReg = delOrReg
                    Names = names
                    Status =
                        [|
                            getStatus part.InstructionTxt
                            getStatus part.InstructionVideo
                        |]
                        
                })
        { model with CurrInstruction = Ok (instruction,id) ;
                     CurrPositions = Some newModInfo }, []
    | PartMsg msg ->
        let (parModel, partModelCmd) = Part.State.update msg model.CurrPart
        { model with CurrPart = parModel}, Cmd.map (PartMsg >> User.Types.InstructionMsg) partModelCmd
    | ErrorMsg str ->
        { model with InstructionErrorMessage =
                        { model.InstructionErrorMessage with Text = str} }, []
    | ModifyInstructionMsg visibility ->
        let disable = if visibility = style.visibility.hidden
                      then true
                      else false
        { model with PartNameModificationInput =
                        { model.PartNameModificationInput with Visible = visibility ; Disable = disable } }, []
    | DeleteButtonEnablenMsg isDisabled ->
        { model with DeleteButton =
                        { model.DeleteButton with Disable = isDisabled } }, []
    | NewModificationInfo (delOrReg,currName,newName) ->
        match model.CurrInstruction with
        | Ok (instruction,id) ->
            match model.CurrPositions with
            | Some modinfo ->
                let (newInstruction,newModInfo) =
                    Logic.updateCurrPositionsTestable instruction
                                                      modinfo
                                                      delOrReg
                                                      currName
                                                      newName
                { model with CurrInstruction = Ok (newInstruction,id)
                             CurrPositions = Some newModInfo}, []
            | _ -> model,[]
        | _ -> model,[]

    | ImplementNewNames positions ->
        match model.CurrInstruction with
        | Ok (instruction,id)->
            let result =
                Logic.implementNewNamesTestable instruction model.CurrPositions positions
            result
            |> function
                | res when res.IsSome ->
                    match res.Value with
                    | Ok (newInstruction,newModInfo) ->
                        {model with CurrInstruction = Ok (newInstruction,id);
                                    CurrPositions = Some newModInfo },[]
                    | Error msg ->
                        model,Cmd.ofMsg(msg)
                | _ -> model, []
        | _ -> model, []

    | UpdateNewName (currName,newName) ->
        match model.CurrPositions with
        | Some currPositions ->
            Instruction.Logic.updateNewNameTestable currPositions
                                                    currName
                                                    newName
            
            |> fun newCurrPos ->
                 { model with CurrPositions = Some newCurrPos }, []
        | _ -> model,[]

    | ResetInstruction str ->
        model, ( str |>
                 ( User.Types.GiveResetInstruction >> Cmd.ofMsg ))
    | NewPage (page,delay) ->
        let msg =
            Elmish.Navigation.Navigation.newUrl(Global.toHash(page))
        model, msg
    | HoverPartMsg (part,utils,visible) ->
        let usrMsg =
            visible
            |> Logic.createHoverMessageCommponents part utils
            |> Logic.hoverMessageFuncChaining

        model, usrMsg

    | ChangeFileStatus(status,utils) ->
        Instruction.Logic.changeFileStatus model status utils
    | SaveInstructionToDataBase utils ->
        console.log(User.Types.CompareNewSaveWithCurrentInstructions)
        console.log(utils)
        match model.CurrInstruction with
        | Ok (instruction,_) ->
            let usrMsg =
                (instruction,model.CurrPositions,utils)
                |> User.Types.CompareNewSaveWithCurrentInstructions
                |> Cmd.ofMsg
            model,usrMsg
        | _ -> model,[]

    | CheckIfSaveFinished(ids,utils,options)->
        let info =
            options
            |> Instruction.Logic.uploadOrDeleteFinished model.CurrPositions
            
        ()
        |>function
            | _ when info.IsSome ->
                let saveToDBIfSomeUploadSuccess =
                    info.Value
                    |> function
                        | (uploadOrDeleteFinished,savingOpt) when savingOpt.IsSome ->
                            let msg =
                                (savingOpt.Value,ids,utils) |>
                                (
                                    SavingResolved >>
                                    NewAdd.Types.CreateNewDataMsg >>
                                    User.Types.NewAddMsg >>
                                    Cmd.ofMsg
                                )

                            { model with CurrPositions = Some uploadOrDeleteFinished},msg
                        | (uploadOrDeleteFinished,_) ->
                            { model with CurrPositions = Some uploadOrDeleteFinished},[] 
                saveToDBIfSomeUploadSuccess
            | _ -> model,[]

    | CreateDeletePopup utils ->
        match model.CurrInstruction with
        | Ok (instr,_) ->
            let msgsIfYesClicked =

                let deleteMsg =
                    (instr,utils)
                    |> User.Types.DeleteInstructionMsg

                let newPossibleInstructionMsg =
                    User.Types.UpdateUserInstructionsType.DeleteInstruction instr
                    |> User.Types.Msg.PossibleNewUserDataMsg

                [|
                    newPossibleInstructionMsg
                    deleteMsg
                |]

            let popupMsgs =
                [|
                    Html.div[
                        prop.className "column"
                        prop.children[
                            Fable.React.Helpers.str "Are you sure you wish to remove instruction?"
                        ]
                    ]
                |]
                
            let msg =
                ( popupMsgs,utils,msgsIfYesClicked)
                |> User.Types.PopUpSettings.DefaultWithOptions
                |> Some
                |> User.Types.Msg.PopUpMsg
                |> Cmd.ofMsg

            model,msg


        | _ -> model, []

    | DeletePartFilesMsg(status) ->
        let msgs =
            Instruction.Logic.deleteProcess status
            |> Cmd.batch
        model,msgs

     | DatabaseChangeMsg(status) ->
        let msgs =
            Instruction.Logic.databaseChangeProcedure status
            |> Cmd.batch

        model,msgs

    | UpdateNewInstructionName instrTitle ->
        match model.CurrInstruction with
        | Ok (instruction,id) ->
            let newInstr =
                (
                    {instruction with Title = instrTitle},
                    id
                )
                |> Ok

            {model with CurrInstruction = newInstr}, []
        | _ -> model,[]

    | NewPendingDatabaseChanges pendingOptions ->
        {model with CurrentDataBaseChanges = pendingOptions}, []

    | CreateDefaultModificationInfoArray ->
        match model.CurrInstruction with
        | Ok (instr,_) ->
            
            let delOrReg  =
                "Delete"
                |> ( Delete >> Some )
            let getStatus name =
                PartStatus.StatusExisting(name)
            let newModInfo =
                instr.Data
                |> Array.map (fun part ->
                    let names =
                        {
                            CurrName = part.Title
                            NewName = None
                        }
                    {
                        DelOrReg = delOrReg
                        Names = names
                        Status =
                            [|
                                getStatus part.InstructionTxt
                                getStatus part.InstructionVideo
                            |]
                        
                    })
            { model with CurrPositions = Some newModInfo }, []
        | _ -> model,[]
