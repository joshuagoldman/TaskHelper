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
open System

let (^&)x = (x)

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

let instructionNameToChange dispatch
                            ( instructionTitle : Data.InstructionTitleInfo )
                            newName =

        let dispatchNewName dbName oldName =
            if newName |> String.length < 30
            then true
            else false
            |> function
                | res when res = true ->
                    let newTitle =
                        {
                            OldName = oldName
                            NewName = newName
                            DbName = dbName
                        }

                    newTitle|>
                    (
                        InstructionTitleInfo.HasNewName >>
                        Instruction.Types.UpdateNewInstructionName >>
                        dispatch
                    )
                | _ -> ()
        match instructionTitle with
        | Data.InstructionTitleInfo.HasOldName title -> 
            dispatchNewName title title
            
        | Data.InstructionTitleInfo.HasNewName titles ->
            dispatchNewName titles.DbName titles.OldName

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
    result

let modifyNames model dispatch utils =
    match model.CurrPositions with
     | Some _ ->
        utils
        |> ( Instruction.Types.ImplementNewNames >>
             dispatch)
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
            
let newNameBecomesCurrent ( modinfos : array<modificationInfo> )
                            positions =
    let divWOSpinner msg =
        [|
            Html.div[
                prop.className "column"
                prop.children[
                    Fable.React.Helpers.str msg
                ]
            ]
        |]

    let funcChainingButton info =
        info |>
        (
            PopUpSettings.DefaultWithButton >>
            Some >>
            User.Types.PopUpMsg
        )
    let notUniqueArr =
        modinfos
        |> Array.choose (fun modInfo -> modInfo.Names.NewName)
        |> Array.choose (fun newName ->
            let notUnique =
                modinfos
                |> Array.tryPick (fun modInfo ->
                    if modInfo.Names.CurrName.Replace(" ","") = newName.Replace(" ","")
                    then Some modInfo
                    else None)
            notUnique)

    ()
    |> function
        | _ when notUniqueArr |> Array.length <> 0 ->
            let notUniqueString =
                notUniqueArr
                |> Array.map (fun modInfo ->
                    modInfo.Names.CurrName)
                |> String.concat ", "

            let msg =
                String.Format(
                    "Part name(s) {0} already exist. Parts must be named such that they are distinct by their name",
                    notUniqueString
                )
            let errorMsg =
                (divWOSpinner msg,positions)
                |> funcChainingButton 

            Error errorMsg
        | _ ->
            let newModInfos =
                modinfos
                |> Array.map (fun modInfo ->
                    ()
                    |> function
                        | _ when modInfo.Names.NewName.IsSome ->
                            { modInfo.Names with NewName = None ; CurrName = modInfo.Names.NewName.Value }
                            |> fun newNames ->
                                { modInfo with Names = newNames}
                        | _ -> modInfo)

            Ok newModInfos

let implementNewNamesTestable ( instruction : Data.InstructionData )
                                modInfoOpt
                                positions =

        let newTitle =
            match instruction.Title with
            | InstructionTitleInfo.HasOldName title ->
                instruction.Title
            | InstructionTitleInfo.HasNewName titles ->
                {
                    NewName = ""
                    OldName = titles.NewName
                    DbName = titles.DbName
                }
                |> InstructionTitleInfo.HasNewName

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
                    let newInstruction =
                        {
                            Title = newTitle
                            Data = parts
                        }
                    let newModinfoRes = newNameBecomesCurrent modInfo positions

                    match newModinfoRes with
                    | Ok newModinfo ->
                        Ok (newInstruction,newModinfo)
                    | Error msg ->
                        Error msg
                    
            Some result
        | _ -> None

let getTitle titleAlt =
    match titleAlt with
    | Data.InstructionTitleInfo.HasOldName title ->
        title
    | Data.InstructionTitleInfo.HasNewName titles ->
        titles.OldName

let newInstructionName ( model: Model<'a> ) titleAlt =

    match titleAlt with
    | Data.InstructionTitleInfo.HasOldName title ->
        match model.CurrInstruction with
        | Ok (existinstruction,_) ->
            match existinstruction.Title with
            | Data.InstructionTitleInfo.HasOldName _ ->
                ""
            | Data.InstructionTitleInfo.HasNewName titlesExist ->
                titlesExist.NewName
        | Error _ -> ""
    | Data.InstructionTitleInfo.HasNewName titles ->
        match model.CurrInstruction with
        | Ok (existinstruction,_) ->
            match existinstruction.Title with
            | Data.InstructionTitleInfo.HasOldName titleExist ->
                if titles.OldName.Replace(" ","") = titleExist.Replace(" ","")
                then
                    titleExist
                else
                    ""
                    
            | Data.InstructionTitleInfo.HasNewName titlesExist ->
                if titles.OldName.Replace(" ","") = titlesExist.OldName.Replace(" ","")
                then
                    titlesExist.NewName
                else
                    ""
        | Error err -> ""
    

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
                                  ( utils : Utilities<'a> )
                                    visible =

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
        Feliz.style.left ( (utils.Ev |> User.Logic.getPositions).X |> int )
        Feliz.style.top ( (utils.Ev |> User.Logic.getPositions).Y |> int )
    |]
    |> fun styles ->
        divs,utils,styles

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

let changeFileStatus ( model : Instruction.Types.Model<User.Types.Msg> )
                     ( newStatus : PartStatus)
                     ( utils ) =
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
                | PartStatus.Uploading (nameComp,_) ->
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

    let divWSpinner msg ( uploaded : float option ) =
        let progressBar =
            match uploaded with
            | Some uploadValue ->
                Html.div[
                    prop.className "column"
                    prop.children[
                        Html.progress[
                            prop.className "progress is-primary"
                            prop.value uploadValue
                            prop.max 100
                            prop.children[
                                str (uploaded |> string)
                            ]
                        ]
                    ]
                ]
            | _ ->
                Html.none
        [|
            Html.div[
                prop.className "column"
                prop.children[
                    Fable.React.Helpers.str msg
                ]
            ]
            User.Logic.spinner
            progressBar
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
                | PartStatus.Uploading (name,uploaded) ->
                    let uploadedAsFloat =
                        match uploaded with
                        | Percentage percentage -> percentage
                        | _ -> 0.0
                    let msgElement =
                        divWSpinner ("Uploading file " + name) ^&Some(uploadedAsFloat)
                    Some msgElement
                | PartStatus.Delete name ->
                    let msgElement =
                        divWSpinner ("Deleting file " + name) None
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
            | PartStatus.Uploading (name,_) ->
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
                    (msgElement,utils)
                    |> funcChainingButton
                    |> Cmd.ofMsg
                | _ ->
                    (msgElement,utils)
                    |> funcChainingNoButton
                    |> Cmd.ofMsg

        { model with CurrPositions = Some newInfo }, msg
    | _ ->  model, []

let deleteProcess ( status : DeleteProcess<string * Utilities<User.Types.Msg>,string * Utilities<User.Types.Msg> * ReactElement> ) =
    match status with
    | DeleteProcess.DeleteInProgress(mediaName,utils) ->
        let mediaToDeleteMsg =
            mediaName
            |> PartStatus.Delete
            |> fun x ->
                (x,utils)
                |> ChangeFileStatus 
                |> User.Types.InstructionMsg
                |> Cmd.ofMsg

        let nextDeleteProcessMsg =
            utils
            |> User.Logic.deleteAsync mediaName 
            |> Cmd.fromAsync
            
        seq[
            mediaToDeleteMsg
            nextDeleteProcessMsg
        ]
    | DeleteProcess.DeleteFinished(mediaName,utils,reactEL) ->
        let mediaToDeleteMsg =
            (mediaName,reactEL)
            |> PartStatus.UploadOrDeleteFinishedSuccesfully 
            |> fun x ->
                (x, utils)
                |> ChangeFileStatus 
                |> User.Types.InstructionMsg
                |> Cmd.ofMsg
        mediaToDeleteMsg
        |> fun x -> seq[x]
        
        
let uploadOrDeleteFinished ( modInfosOpt : Instruction.Types.modificationInfo [] option) options =
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
                            | PartStatus.Uploading (name,_) ->
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

let databaseChangeProcedure  ( status :  DatabaseChangeProcess<array<Data.DatabaseSavingOptions> * Data.DBIds * Utilities<User.Types.Msg>,
                                                               DatabaseChangeResult<User.Types.Msg>> ) =
    match status with
    | DatabaseChangeBegun(dbSaveOpt,ids,utils) ->
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
            (divWSpinner,utils)
            |> buttonFuncChaining
            |> Cmd.ofMsg

        let dbChangeMsg =
            utils
            |> User.Logic.sqlCommandToDB dbSaveOpt ids
            |> Cmd.fromAsync
            |> fun x -> [|x|]

        dbChangeMsg
        |> Array.append ([|popupMsg|])
    | DatabseChangeFinished(DatabaseChangeFailed(msg,utils)) ->
        let funcChaining positions msg =
            ([|msg|],utils) |>
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
            |> funcChaining utils

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

let saveNewData ( medias : (NewAdd.Types.MediaChoiceFormData * string)[] )
                ( options : DatabaseNewFilesOptions )
                dbIds
                utils =

    let fullPath name =
        String.Format(
            "User_{0}/Instruction_{1}/{2}",
            dbIds.UserId,
            dbIds.InstructionId,
            name
        )

    let matchMaking str =
        medias
        |> Array.map (fun (_,newName) -> fullPath newName)
        |> Array.exists (fun nameComp ->
                nameComp = str)

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
            |> Array.map (fun (media,newName) ->
                match media with
                | NewAdd.Types.MediaChoiceFormData.Video file -> (file,newName)
                | NewAdd.Types.MediaChoiceFormData.InstructionTxt file -> (file,newName))
            |> Array.map (fun media ->
                (media,dbIds,utils,options)
                |> funcChaining
                |> Cmd.ofMsg)
            |> Cmd.batch
            |> fun msg ->
                msg
        | _ ->
            "Not all necesarry media exist!"
            |> User.Logic.errorPopupMsg utils
            |> Cmd.ofMsg
            |> fun msg ->
                msg

let isInstructionUnique instruction currentInstructions =
    match instruction.Title with
    | Data.InstructionTitleInfo.HasOldName titleFromDataBase ->
        let isInstructionUnique =
            currentInstructions
            |> Array.forall (fun instructionComp ->
                match instructionComp.Title with
                | Data.InstructionTitleInfo.HasOldName title ->
                    title.Replace(" ","") <> titleFromDataBase.Replace(" ","")
                | Data.InstructionTitleInfo.HasNewName titles ->
                    titles.OldName.Replace(" ","") <> titleFromDataBase.Replace(" ",""))

        isInstructionUnique
    | Data.InstructionTitleInfo.HasNewName titles ->
        let isInstructionUnique =
            currentInstructions
            |> Array.forall (fun instructionComp ->
                match instructionComp.Title with
                | Data.InstructionTitleInfo.HasOldName title ->
                    title.Replace(" ","") <> titles.OldName.Replace(" ","")
                | Data.InstructionTitleInfo.HasNewName titles ->
                    titles.OldName.Replace(" ","") <> titles.OldName.Replace(" ",""))

        isInstructionUnique

let updateUserInstructions possibInstrOpt currentInstructions id =
    match possibInstrOpt with
    | NewPossibleInstructionOptions.SaveOrDeleteAttempt updateType ->
        match updateType with
        | UpdateUserInstructionsType.AddNewInstruction instruction ->
            let (isInstrunctionUnique, newInstruction) =
                match instruction.Title with
                | Data.InstructionTitleInfo.HasOldName titleFromDataBase ->
                    let isInstructionUnique =
                        currentInstructions
                        |> Array.forall (fun instructionComp ->
                            match instructionComp.Title with
                            | Data.InstructionTitleInfo.HasOldName title ->
                                title.Replace(" ","") <> titleFromDataBase.Replace(" ","")
                            | Data.InstructionTitleInfo.HasNewName titles ->
                                let isUnique = titles.OldName.Replace(" ","") <> titleFromDataBase.Replace(" ","")
                                isUnique
                                )

                    (isInstructionUnique,instruction)
                | Data.InstructionTitleInfo.HasNewName titles ->
                    let isInstructionUnique =
                        currentInstructions
                        |> Array.forall (fun instructionComp ->
                            match instructionComp.Title with
                            | Data.InstructionTitleInfo.HasOldName title ->
                                title.Replace(" ","") <> titles.OldName.Replace(" ","")
                            | Data.InstructionTitleInfo.HasNewName titlesComp ->
                                let isUnique = titlesComp.OldName.Replace(" ","") <> titles.OldName.Replace(" ","")
                                isUnique)

                    let instructionMadeIntoNoNewName =
                        {
                            Title = 
                                titles.NewName
                                |> InstructionTitleInfo.HasOldName

                            Data = instruction.Data

                        }

                    (isInstructionUnique,instructionMadeIntoNoNewName)

            ()
            |> function
                | _ when isInstrunctionUnique = true ->
                    let newInstructions =
                        [|newInstruction|]
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
                    |> isInstructionUnique instruction

                ()
                |> function
                    | _ when isInstructionUnique = false ->
                        let newInstructions =
                            match instruction.Title with
                            | Data.InstructionTitleInfo.HasOldName titleFromDataBase ->
                                let filteredInstructions =
                                    currentInstructions
                                    |> Array.filter (fun instructionComp ->
                                        match instructionComp.Title with
                                        | Data.InstructionTitleInfo.HasOldName title ->
                                            title.Replace(" ","") <> titleFromDataBase.Replace(" ","")
                                        | Data.InstructionTitleInfo.HasNewName titles ->
                                            let isUnique = titles.OldName.Replace(" ","") <> titleFromDataBase.Replace(" ","")
                                            isUnique
                                            )

                                filteredInstructions
                            | Data.InstructionTitleInfo.HasNewName titles ->
                                let filteredInstructions =
                                    currentInstructions
                                    |> Array.filter (fun instructionComp ->
                                        match instructionComp.Title with
                                        | Data.InstructionTitleInfo.HasOldName title ->
                                            title.Replace(" ","") <> titles.OldName.Replace(" ","")
                                        | Data.InstructionTitleInfo.HasNewName titlesComp ->
                                            let isUnique = titlesComp.OldName.Replace(" ","") <> titles.OldName.Replace(" ","")
                                            isUnique)

                                filteredInstructions

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
                |> isInstructionUnique instruction

            let newInstructions =
                match instruction.Title with
                | Data.InstructionTitleInfo.HasOldName titleFromDataBase ->
                    let filteredInstructions =
                        currentInstructions
                        |> Array.map (fun instructionComp ->
                            match instructionComp.Title with
                            | Data.InstructionTitleInfo.HasOldName title ->
                                let hasSameTitle =
                                    title.Replace(" ","") = titleFromDataBase.Replace(" ","")

                                ()
                                |> function
                                    | _ when hasSameTitle = true ->
                                        instructionComp
                                    | _ ->  instruction
                            | Data.InstructionTitleInfo.HasNewName titles ->
                                let hasSameTitle =
                                    titles.OldName.Replace(" ","") = titleFromDataBase.Replace(" ","")

                                ()
                                |> function
                                    | _ when hasSameTitle = true ->
                                        instructionComp
                                    | _ ->  instruction)

                    filteredInstructions
                | Data.InstructionTitleInfo.HasNewName titles ->
                    let updatedInstructions =
                        currentInstructions
                        |> Array.map (fun instructionComp ->
                            match instructionComp.Title with
                            | Data.InstructionTitleInfo.HasOldName title ->
                                let hasSameTitle =
                                    titles.OldName.Replace(" ","") = title.Replace(" ","")

                                ()
                                |> function
                                    | _ when hasSameTitle = true ->
                                        instructionComp
                                    | _ ->  instruction
                            | Data.InstructionTitleInfo.HasNewName titlesComp ->
                                let hasSameTitle =
                                    titles.OldName.Replace(" ","") = titlesComp.OldName.Replace(" ","")

                                ()
                                |> function
                                    | _ when hasSameTitle = true ->
                                        instructionComp
                                    | _ ->  instruction)

                    updatedInstructions

            ()
            |> function
                | _ when isInstructionUnique = false ->

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
        
                
            
        
