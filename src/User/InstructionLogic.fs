module Instruction.Logic

open Fable.Core
open Fable.React
open Fable.React.Props
open Instruction.Types
open Feliz
open Browser
open Fable.Core.JsInterop
open Elmish.Navigation
open Elmish
open User.Types
open Fable.SimpleHttp
open Data


let modifyOrNot model dispatch =
    model.PartNameModificationInput.Visible
    |> function
        | res when res = style.visibility.hidden ->
            style.visibility.visible |>
            Instruction.Types.ModifyInstructionMsg
            |> fun msg1 ->
                    [|
                        msg1
                        (false |>
                         Instruction.Types.DeleteButtonEnablenMsg)
                    |]
                    
            |> Array.iter (fun msg -> msg |> dispatch)

        | _ -> style.visibility.hidden |>
                Instruction.Types.ModifyInstructionMsg
                |> fun msg1 ->
                        [|
                            msg1
                            (true |>
                             Instruction.Types.DeleteButtonEnablenMsg)
                        |]
                        
                |> Array.iter (fun msg -> msg |> dispatch)

let go2PartFromInstruction part instruction dispatch =
        Part.Types.NewPart2Show (part,instruction)
        |> Instruction.Types.PartMsg
        |> dispatch
        |> fun _ ->
            Part.Logic.go2PreviousOrNext instruction part.Title (Instruction.Types.PartMsg >>
                                                                 dispatch) ""

let newModInfo4Debugging newModinfo =
    newModinfo
    |> Array.map (fun info ->
        ()
        |> function
            | _ when info.DelOrReg.IsSome ->
                match info.DelOrReg.Value with
                | Delete str -> str
                | Regret str -> str
            | _ -> ""
            |> fun deloReg ->
                info.Names.CurrName +
                "\n" +
                deloReg +
                "\n" +
                (if info.Names.NewName.IsSome then info.Names.NewName.Value else "")  +
                "\n\n")
            |> String.concat ""

let ajustAfterNewModinfo ( currInstruction : Data.InstructionData )
                         ( newModinfo : array<modificationInfo> ) :
                           ( Data.InstructionData * array<modificationInfo> ) =
    newModinfo
    |> Array.map (fun info ->
        currInstruction.Data
        |> Array.tryFind (fun part ->
            part.Title.Trim() = info.Names.CurrName.Trim())
        |> function
            | res when res.IsSome ->
                Some res.Value
            | _ -> None)
    |> Array.choose id
    |> fun parts ->
        let newParts4Debugging =
            parts
            |> Array.map (fun part ->
                part.Title + "\n")
            |> String.concat ""
        { currInstruction with Data = parts}, newModinfo

let updateCurrPositionsTestable (currInstruction : Data.InstructionData)
                        ( currModInfo : array<modificationInfo> )
                        ( delOrReg : DeleteInfo Option )
                          currName
                        ( newName : Option<string> ) :
                        Data.InstructionData *
                        array<modificationInfo> =
    ()
    |> function
        | _ when delOrReg.IsSome && newName.IsNone ->
            currModInfo
            |> Array.map (fun info ->
                info.Names.CurrName
                |> function
                    | name when name = currName ->
                        info.DelOrReg
                        |> function
                            | res when res.IsSome ->
                                let newVal =
                                    match res.Value with
                                    | Delete _ ->
                                        Regret("Regret")
                                    | Regret _ ->
                                        Delete("Delete")
                                { info with DelOrReg = Some newVal }
                            | _ -> info
                    | _ -> info
                )
            |> fun newModInfo ->
                newModInfo
                |> ajustAfterNewModinfo currInstruction
        | _ when newName.IsSome && delOrReg.IsNone ->
            let isNewNameValid =
                currInstruction.Data
                |> Array.exists (fun part ->
                    part.Title.Trim() = newName.Value)

            let getNewNameStatus ( name : string ) =
                currModInfo
                |> Array.pick (fun modInfo ->
                    ()
                    |> function
                        | _ when modInfo.Names.CurrName.Trim() = name.Trim() ->
                            Some modInfo.Status
                        | _ -> None)

            ()
            |> function
                | _ when isNewNameValid = true ->
                    currModInfo
                    |> Array.map (fun info ->
                        ()
                        |> function
                            | _ when info.Names.CurrName.Trim() = currName.Trim() ->
                                { info with Names = {
                                                CurrName = newName.Value
                                                NewName = None
                                            }
                                            Status = getNewNameStatus newName.Value }
                            | _ when info.Names.CurrName.Trim() = newName.Value.Trim() ->
                                { info with Names = {
                                                CurrName = currName
                                                NewName = None
                                            }
                                            Status = getNewNameStatus currName}
                            | _ -> info
                        )
                        
                    |>  fun newModinfo ->
                        newModinfo
                        |> ajustAfterNewModinfo currInstruction
                | _ -> currInstruction,currModInfo
        | _ -> currInstruction, currModInfo

let getCurrentDelOrReg model ( part : Data.partData ) =
    match model.CurrPositions with
    | Some modinfo ->
        modinfo
        |> Array.tryFind (fun info ->
            info.Names.CurrName.Trim() = part.Title.Trim())
        |> function
            | res when res.IsSome ->
                res.Value |> Some
            | _ -> None
    | None -> None

let dispatchDelOrReg model dispatch ( part : Data.partData ) =
    getCurrentDelOrReg model part
    |> function
        | res when res.IsSome ->
            (res.Value.DelOrReg, res.Value.Names.CurrName,None)
            |> (Instruction.Types.NewModificationInfo >> dispatch)
        | _ -> ()

let getDelOrRegName model dispatch ( part : Data.partData ) =
        getCurrentDelOrReg model part
        |> function
            | res when res.IsSome ->
                res.Value
                |> function
                    | subRes when subRes.DelOrReg.IsSome ->
                        match subRes.DelOrReg.Value with
                        | Delete strVal ->
                            Some strVal
                        | Regret strVal ->
                            Some strVal
                    | _ -> None
            | _ -> None

let newPartSelected ( ev : Types.Event ) partName dispatch =
    let newPartName = ev.target?value |> string

    (None,partName,Some newPartName)
    |> Instruction.Types.NewModificationInfo
    |> fun x -> [|x|]
    |> Array.append (
            [|
                Global.UserPage.Instruction
                |> Global.Page.User
                |> fun page ->
                    (page,None)
                    |> NewPage
            |]
        )
    |> Array.iter (fun msg -> msg |> dispatch)

let partNameToChange dispatch
                     ( part : Data.partData )
                       newPartName =
        if newPartName |> String.length < 30
        then true
        else false
        |> function
            | res when res = true ->
                (part.Title,newPartName)
                |> ( Instruction.Types.UpdateNewName >> dispatch )
            | _ -> ()
let updateNewNameTestable currPositions ( currName : string ) newName=
    let result =
        currPositions
        |> Array.map (fun modInfo ->
            ()
            |> function
                | _ when modInfo.Names.CurrName.Trim() = currName.Trim() ->
                    { modInfo.Names with NewName = Some newName}
                    |> fun newModInfo ->
                        { modInfo with Names = newModInfo }
                | _  -> modInfo)
    let ddd =
        newModInfo4Debugging result

    result

let modifyNames model dispatch =
    match model.CurrPositions with
     | Some _ ->
        (Instruction.Types.ImplementNewNames |> dispatch)
     | _ -> ()

let enableModificationTestable ( modInfoSeq : array<modificationInfo> Option )
                               ( part : Data.partData )  =
    ()
    |> function
        | _ when modInfoSeq.IsSome ->
            modInfoSeq.Value
            |> Array.tryFind (fun modInfo ->
                modInfo.Names.CurrName.Trim() = part.Title.Trim())
            |> function
                | res when res.IsSome ->
                    res.Value.DelOrReg
                    |> function
                        | newRes when newRes.IsSome ->
                            match newRes.Value with
                            | Delete _ ->
                                true
                            | Regret _ -> false
                        | _ -> false
                | _ -> false
        | _ -> false
            
let newNameBecomesCurrent ( modinfo : array<modificationInfo> ) =
    modinfo
    |> Array.map (fun info ->
        ()
        |> function
            | _ when info.Names.NewName.IsSome ->
                { info.Names with NewName = None ; CurrName = info.Names.NewName.Value }
                |> fun newNames ->
                    { info with Names = newNames}
            | _ -> info)

let implementNewNamesTestable ( instruction : Data.InstructionData )
                                modInfoOpt =
        match modInfoOpt with
        | Some modInfo ->
            let result =
                instruction.Data
                |> Array.map (fun part ->
                    modInfo
                    |> Array.tryFind (fun info ->
                        info.Names.CurrName.Trim() = part.Title.Trim() &&
                        info.Names.NewName.IsSome)
                    |> function
                        | res when res.IsSome ->
                            { part with Title = res.Value.Names.NewName.Value}
                        | _ -> part)
                |> fun parts ->
                    let newInstruction = {instruction with Data = parts}
                    let newModinfo = newNameBecomesCurrent modInfo
                    (newInstruction,newModinfo)
            Some result
        | _ -> None

let newnameValue model ( part : Data.partData ) =
    match model.CurrPositions with
    | Some modificationInfo ->
        modificationInfo
        |> Array.tryFind (fun info ->
            info.Names.CurrName.Trim() = part.Title.Trim())
        |> function
            | res when res.IsSome ->
                match res.Value.Names.NewName with
                | Some newName ->
                    newName 
                | _ -> ""
            | _ -> ""
    | _ -> ""

let hoverMessageFuncChaining args =
    args |>
    (
        User.Types.OptionalWithMsg >>
        Some >>
        User.Types.PopUpMsg >>
        Cmd.ofMsg
    )

let createHoverMessageCommponents ( part : Data.partData )
                                  ( ev : Types.MouseEvent )
                                    visible =
    let positions =
        {
            Data.Position.X = ( ev?pageX : float )
            Data.Position.Y = ( ev?pageY : float )
        }

    let style =
        prop.style[
            style.fontSize 10
            style.color.black
            style.fontWeight.bold
            style.margin 1
        ]

    let divs =
        [|
            Global.divWithStyle
                    (Some "columns is-gapless")
                    ("Instruction Video: " + part.InstructionVideo.Replace("Videos/",""))
                    style

            Global.divWithStyle
                    (Some "columns is-gapless")
                    ("Instruction Text: " + part.InstructionTxt)
                    style
        |]

    [|
        visible
        Feliz.style.left ( positions.X |> int )
        Feliz.style.top ( positions.Y |> int )
    |]
    |> fun styles ->
        divs,positions,styles

let filenameWStatus (msg : array<ReactElement> ) =
    Html.div[
        prop.className "columns is-centered"
        prop.style[
            style.fontWeight.bold
            style.fontSize 12
            style.maxWidth 1500
            style.margin 5
            ]
        prop.children(
            msg
        )  
    ]

let changeFileStatus ( model : Instruction.Types.Model )
                     ( newStatus : PartStatus)
                     ( positions ) =
    let upDatNameIfMatch ( name : string )
                         ( nameComp : string )
                           currStatus =
        ()
        |> function
            | _ when name.Replace(" ","") = nameComp.Replace(" ","") ->
                newStatus
            | _ -> currStatus

    let getNewModInfos ( modInfos : array<modificationInfo> ) name =
        modInfos
        |> Array.map (fun modInfo ->
            modInfo.Status
            |> Array.map (fun currPartStatus ->
                match currPartStatus with
                | PartStatus.Uploading nameComp ->
                    upDatNameIfMatch name nameComp currPartStatus
                | PartStatus.Delete nameComp ->
                    upDatNameIfMatch name nameComp currPartStatus
                | PartStatus.StatusExisting nameComp ->
                    upDatNameIfMatch name nameComp currPartStatus
                | PartStatus.UploadOrDeleteFinishedSuccesfully (nameComp,_) ->
                    upDatNameIfMatch name nameComp currPartStatus
                | PartStatus.UploadOrDeleteFinishedWithFailure (nameComp,_) ->
                    upDatNameIfMatch name nameComp currPartStatus)
            |> fun newStatuses ->
                { modInfo with Status = newStatuses })

    let divWSpinner msg =
        [|
            Html.div[
                prop.className "column"
                prop.children[
                    Fable.React.Helpers.str msg
                ]
            ]
            User.Logic.spinner
        |]

    let funcChainingButton info =
        info |>
        (
            PopUpSettings.DefaultWithButton >>
            Some >>
            User.Types.PopUpMsg
        )

    let funcChainingNoButton info =
        info |>
        (
            PopUpSettings.Default >>
            Some >>
            User.Types.PopUpMsg
        )

    let newStatusReactElements ( modInfos : array<modificationInfo> ) =
        modInfos
        |> Array.collect (fun modInfo ->
            modInfo.Status
            |> Array.choose (fun status ->
                match status with
                | PartStatus.UploadOrDeleteFinishedSuccesfully(_,msgElement) ->
                    Some ([|msgElement|])
                | PartStatus.UploadOrDeleteFinishedWithFailure(_,msgElement) ->
                    Some ([|msgElement|])
                | PartStatus.Uploading name ->
                    let msgElement =
                        divWSpinner ("Uploading file " + name)
                    Some msgElement
                | PartStatus.Delete name ->
                    let msgElement =
                        divWSpinner ("Deleting file " + name)
                    Some msgElement
                | PartStatus.StatusExisting _ ->
                    None)
            |> Array.collect(fun x -> x))

    match model.CurrPositions  with
    | Some modInfos ->
        let (newInfo,msgElement) =
            match newStatus with
            | PartStatus.UploadOrDeleteFinishedSuccesfully(name,_) ->
                let newModInfos = getNewModInfos modInfos name
                let newFileStatusMsgs = newStatusReactElements newModInfos
                (newModInfos,newFileStatusMsgs)
            | PartStatus.UploadOrDeleteFinishedWithFailure(name,_) ->
                let newModInfos = getNewModInfos modInfos name
                let newFileStatusMsgs = newStatusReactElements newModInfos
                (newModInfos,newFileStatusMsgs)
            | PartStatus.Uploading name ->
                let newModInfos =
                    getNewModInfos modInfos name
                let newFileStatusMsgs = newStatusReactElements newModInfos
                (newModInfos,newFileStatusMsgs)
            | PartStatus.Delete name ->
                let newModInfos =
                    getNewModInfos modInfos name
                let newFileStatusMsgs = newStatusReactElements newModInfos
                (newModInfos,newFileStatusMsgs)
            | PartStatus.StatusExisting name ->
                let newModInfos =
                    getNewModInfos modInfos name
                let newFileStatusMsgs = newStatusReactElements newModInfos
                (newModInfos,newFileStatusMsgs)

        let allFileStatusesAreStale =
            newInfo
            |> Array.collect (fun modInfo ->
                modInfo.Status
                |> Array.map (fun status ->
                    match status with
                    | PartStatus.UploadOrDeleteFinishedSuccesfully(_,_) ->
                        true
                    | PartStatus.UploadOrDeleteFinishedWithFailure(_,_) ->
                        true
                    | PartStatus.StatusExisting _ ->
                        true
                    | _ -> false))
            |> Array.forall (fun x -> x)

        let msg =
            ()
            |> function
                | _ when allFileStatusesAreStale = true ->
                    (msgElement,positions)
                    |> funcChainingButton
                    |> Cmd.ofMsg
                | _ ->
                    (msgElement,positions)
                    |> funcChainingNoButton
                    |> Cmd.ofMsg

        { model with CurrPositions = Some newInfo }, msg
    | _ ->  model, []

let deleteProcess ( status : DeleteProcess<string * Data.Position,string * Data.Position * ReactElement> ) =
    match status with
    | DeleteProcess.DeleteInProgress(mediaName,positions) ->
        let mediaToDeleteMsg =
            mediaName
            |> PartStatus.Delete
            |> fun x ->
                (x,positions)
                |> ChangeFileStatus 
                |> User.Types.InstructionMsg
                |> Cmd.ofMsg

        let nextDeleteProcessMsg =
            positions
            |> User.Logic.deleteAsync mediaName 
            |> Cmd.fromAsync
            
        seq[
            mediaToDeleteMsg
            nextDeleteProcessMsg
        ]
    | DeleteProcess.DeleteFinished(mediaName,positions,reactEL) ->
        let mediaToDeleteMsg =
            (mediaName,reactEL)
            |> PartStatus.UploadOrDeleteFinishedSuccesfully 
            |> fun x ->
                (x, positions)
                |> ChangeFileStatus 
                |> User.Types.InstructionMsg
                |> Cmd.ofMsg
        mediaToDeleteMsg
        |> fun x -> seq[x]
        
        
let uploadOrDeleteFinished modInfosOpt options =
    match modInfosOpt with
    | Some modInfos ->
        modInfos
        |> Array.collect (fun modInfo ->
            modInfo.Status
            |> Array.map (fun status ->
                match status with
                | PartStatus.UploadOrDeleteFinishedSuccesfully(_,_) ->
                    true
                | PartStatus.UploadOrDeleteFinishedWithFailure(_,_) ->
                    true
                | PartStatus.StatusExisting _ ->
                    true
                | _ -> false))
        |> Array.forall (fun res -> res)
        |> function
            | uploadOrDeleteFinished when uploadOrDeleteFinished = true ->
                let modInfosNew =
                    modInfos 
                    |> Array.map (fun modInfo ->
                        modInfo.Status
                        |> Array.map (fun status ->
                            match status with
                            | PartStatus.UploadOrDeleteFinishedSuccesfully(name,_) ->
                                PartStatus.StatusExisting name
                            | PartStatus.UploadOrDeleteFinishedWithFailure(name,_) ->
                                PartStatus.StatusExisting name
                            | PartStatus.StatusExisting name ->
                                PartStatus.StatusExisting name
                            | PartStatus.Uploading name ->
                                PartStatus.StatusExisting name
                            | PartStatus.Delete name ->
                                PartStatus.StatusExisting name)
                        |> fun newStatuses ->
                            { modInfo with Status = newStatuses}
                        )

                let partsToAddToDB =
                    modInfos
                    |> Array.choose (fun modInfo ->
                        modInfo.Status
                        |> Array.forall (fun status ->
                            match status with
                            | PartStatus.UploadOrDeleteFinishedSuccesfully(_,_) ->
                                true
                            | _ -> false)
                        |> function
                            | isSuccessUpload when isSuccessUpload = true ->
                                Some modInfo.Names.CurrName
                            | _ -> None)
                    |> function
                        | successUploads when successUploads |> Array.length <> 0 ->
                            Some successUploads
                        | _ -> None

                let newInstructionInfoForDB =
                    ()
                    |>function
                        | _ when partsToAddToDB.IsSome ->
                            let instruction4NewDBInfo =
                                match options with
                                | DatabaseNewFilesOptions.SameInstructionOption instruction ->
                                    instruction.Data
                                    |> Array.choose (fun part ->
                                        partsToAddToDB.Value
                                        |> Array.exists (fun partCompTitle ->
                                            partCompTitle.Replace(" ","") = part.Title.Replace(" ",""))
                                        |> function
                                            | existsPartToAddToDB when existsPartToAddToDB = true ->
                                                Some part
                                            | _ -> None)
                                    |> function
                                        | existsPartsToAddToDB when existsPartsToAddToDB |> Array.length <> 0 ->
                                            let instruction4DBInfo =
                                                {
                                                    Title = instruction.Title
                                                    Data = existsPartsToAddToDB
                                                }

                                            let savingOptions =
                                                instruction4DBInfo
                                                |> DatabaseNewFilesOptions.SameInstructionOption
                                                |> DatabaseSavingOptions.NewFilesInstruction

                                            Some([|savingOptions|])
                                        | _ -> None
                                | DatabaseNewFilesOptions.NewInstructionOption instruction ->
                                    instruction.Data
                                    |> Array.choose (fun part ->
                                        partsToAddToDB.Value
                                        |> Array.exists (fun partCompTitle ->
                                            partCompTitle.Replace(" ","") = part.Title.Replace(" ",""))
                                        |> function
                                            | existsPartToAddToDB when existsPartToAddToDB = true ->
                                                Some part
                                            | _ -> None)
                                    |> function
                                        | existsPartsToAddToDB when existsPartsToAddToDB |> Array.length <> 0 ->
                                            let instruction4DBInfo =
                                                {
                                                    Title = instruction.Title
                                                    Data = existsPartsToAddToDB 
                                                }

                                            let savingOptions =
                                                instruction4DBInfo
                                                |> DatabaseNewFilesOptions.NewInstructionOption
                                                |> DatabaseSavingOptions.NewFilesInstruction

                                            Some([|savingOptions|])
                                        | _ -> None

                            instruction4NewDBInfo
                        | _ -> None
                Some (modInfosNew,newInstructionInfoForDB)
            | _ -> None
    | _ -> None

let databaseChangeProcedure  ( status :  DatabaseChangeProcess<array<Data.DatabaseSavingOptions> * Data.DBIds * Data.Position,
                                                               DatabaseChangeResult> ) =
    match status with
    | DatabaseChangeBegun(dbSaveOpt,ids,positions) ->
        let databaseChangesMsg =
            "Performing database changes..."

        let divWSpinner =
            [|
                Html.div[
                    prop.className "column"
                    prop.children[
                        Fable.React.Helpers.str databaseChangesMsg
                    ]
                ]
                User.Logic.spinner
            |]
        let buttonFuncChaining info =
            info |>
            (
                PopUpSettings.Default >>
                Some >>
                User.Types.PopUpMsg
            )

        let popupMsg =
            (divWSpinner,positions)
            |> buttonFuncChaining
            |> Cmd.ofMsg

        let dbChangeMsg =
            positions
            |> User.Logic.sqlCommandToDB dbSaveOpt ids
            |> Cmd.fromAsync
            |> fun x -> [|x|]

        dbChangeMsg
        |> Array.append ([|popupMsg|])
    | DatabseChangeFinished(DatabaseChangeFailed(msg,positions)) ->
        let funcChaining positions msg =
            ([|msg|],positions) |>
            (
                PopUpSettings.Default >>
                Some >>
                User.Types.PopUpMsg >>
                Cmd.ofMsg
            )

        let funcChainingDelayedPopupKill =
            None 
            |> User.Types.PopUpMsg
            |> User.Logic.delayedMessage 3000
            |> Cmd.fromAsync

        let databaseChangePopupMsg =
            msg
            |> funcChaining positions

        let databaseMsgsCombined =
            [|databaseChangePopupMsg ; funcChainingDelayedPopupKill|]

        databaseMsgsCombined

    | DatabseChangeFinished(DatabaseChangeSucceeded(msg,positions,databaseOptions)) ->
        let funcChaining positions msg =
            ([|msg|],positions) |>
            (
                PopUpSettings.Default >>
                Some >>
                User.Types.PopUpMsg >>
                Cmd.ofMsg
            )

        let funcChainingDelayedPopupKill =
            None 
            |> User.Types.PopUpMsg
            |> User.Logic.delayedMessage 3000
            |> Cmd.fromAsync

        let databaseChangePopupMsg =
            msg
            |> funcChaining positions

        let databaseMsgsCombined =
                [|
                    User.Types.NewUserDataToAddMsg
                    |> Cmd.ofMsg

                    databaseChangePopupMsg
                    funcChainingDelayedPopupKill
                |]

        let deletePartMsgs parts =
            parts
            |> Array.collect (fun part ->
                let startDelProcessInstructionTxt =
                    (part.InstructionVideo,positions)
                    |> Instruction.Types.DeleteInProgress
                    |> Instruction.Types.DeletePartFilesMsg
                    |> User.Types.InstructionMsg
                    |> Cmd.ofMsg

                let startDelProcessInstructionVideo =
                    (part.InstructionTxt,positions)
                    |> Instruction.Types.DeleteInProgress
                    |> Instruction.Types.DeletePartFilesMsg
                    |> User.Types.InstructionMsg
                    |> Cmd.ofMsg
                [|
                    startDelProcessInstructionVideo
                    startDelProcessInstructionTxt
                |])

        let ifDeleteMsg =
            databaseOptions
            |> Array.collect (fun option ->
                match option with
                | PartsToDeleteInstruction delOptions ->
                    match delOptions with
                    | DatabaseDeleteOptions.DeleteInstruction instr ->
                        instr.Data
                        |> deletePartMsgs
                        |> Array.append databaseMsgsCombined
                    | DatabaseDeleteOptions.DeleteParts instr ->
                        instr.Data
                        |> deletePartMsgs
                        |> Array.append databaseMsgsCombined
                | _ ->
                    databaseMsgsCombined)
        ifDeleteMsg

let saveNewData newInstrDataOpt
              ( options : DatabaseNewFilesOptions )
                dbIds
                positions =
    match newInstrDataOpt with
    | Some medias ->
        let matchMaking str =
            medias
            |> Array.exists (fun media ->
                match media with
                | NewAdd.Types.Video vid ->
                    vid.name = str
                | NewAdd.Types.InstructionTxt instr ->
                    instr.name = str)

        let newInstr =
            match options with
            | DatabaseNewFilesOptions.NewInstructionOption instr ->
                instr
            | DatabaseNewFilesOptions.SameInstructionOption instr ->
                instr

        newInstr.Data
        |> Array.forall (fun part ->
            matchMaking part.InstructionTxt &&
            matchMaking part.InstructionVideo)
        |> function
            | res when res = true ->
                let funcChaining info =
                    info |>
                    (
                        SavingHasNostStartedYet >>
                        NewAdd.Types.CreateNewDataMsg >>
                        User.Types.NewAddMsg
                    )
                medias
                |> Array.map (fun media ->
                    match media with
                    | NewAdd.Types.MediaChoiceFormData.Video file -> file
                    | NewAdd.Types.MediaChoiceFormData.InstructionTxt file -> file)
                |> Array.map (fun file ->
                    (file,dbIds,positions,options)
                    |> funcChaining
                    |> Cmd.ofMsg)
                |> Cmd.batch
                |> fun msg ->
                    msg
            | _ ->
                "Not all necesarry media exist!"
                |> User.Logic.errorPopupMsg positions
                |> Cmd.ofMsg
                |> fun msg ->
                    msg
    | None -> []


let updateUserInstructions possibInstrOpt currentInstructions id =
    match possibInstrOpt with
    | NewPossibleInstructionOptions.SaveOrDeleteAttempt updateType ->
        match updateType with
        | UpdateUserInstructionsType.AddNewInstruction instruction ->
            let isInstructionUnique =
                currentInstructions
                |> Array.forall (fun instructionComp ->
                    instructionComp.Title.Replace(" ","") <> instruction.Title.Replace(" ",""))

            ()
            |> function
                | _ when isInstructionUnique = true ->
                    let newInstructions =
                        [|instruction|]
                        |> Array.append currentInstructions

                    {
                        Id = id
                        Instructions = newInstructions

                    }
                    |> ( Ok >> Deferred.Resolved )
                    |> fun newUserData ->
                        Some newUserData
                | _ ->
                    None
                
        | UpdateUserInstructionsType.DeleteInstruction instruction ->
                let isInstructionUnique =
                    currentInstructions
                    |> Array.forall (fun instructionComp ->
                        instructionComp.Title.Replace(" ","") <> instruction.Title.Replace(" ",""))

                ()
                |> function
                    | _ when isInstructionUnique = false ->
                        let newInstructions =
                            currentInstructions
                            |> Array.filter (fun instructionCompare ->
                                    instructionCompare.Title.Replace(" ","") <> instruction.Title.Replace(" ","")) 

                        {
                            Id = id
                            Instructions = newInstructions

                        }
                        |> ( Ok >> Deferred.Resolved )
                        |> fun newUserData ->
                            Some newUserData
                    | _ ->
                        None
                
        | UpdateUserInstructionsType.UpdateInstruction instruction ->
            let isInstructionUnique =
                currentInstructions
                |> Array.forall (fun instructionComp ->
                    instructionComp.Title.Replace(" ","") <> instruction.Title.Replace(" ",""))

            ()
            |> function
                | _ when isInstructionUnique = false ->
                    let newInstructions =
                        currentInstructions
                        |> Array.map (fun instructionCompare ->
                            let hasSameTitle =
                                instructionCompare.Title.Replace(" ","") <> instruction.Title.Replace(" ","")

                            ()
                            |> function
                                | _ when hasSameTitle = true ->
                                    instruction
                                | _ -> instructionCompare)

                    {
                        Id = id
                        Instructions = newInstructions

                    }
                    |> ( Ok >> Deferred.Resolved )
                    |> fun newUserData ->
                        Some newUserData
                | _ ->
                    None
    | NewPossibleInstructionOptions.NoSaveOrDeleteAttempt ->
        None
        
                
            
        
