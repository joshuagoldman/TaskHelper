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
                    seq[
                        msg1
                        (false |>
                         Instruction.Types.DeleteButtonEnablenMsg)
                    ]
                    
            |> Seq.iter (fun msg -> msg |> dispatch)

        | _ -> style.visibility.hidden |>
                Instruction.Types.ModifyInstructionMsg
                |> fun msg1 ->
                        seq[
                            msg1
                            (true |>
                             Instruction.Types.DeleteButtonEnablenMsg)
                        ]
                        
                |> Seq.iter (fun msg -> msg |> dispatch)

let go2PartFromInstruction part instruction dispatch =
        Part.Types.NewPart2Show (part,instruction)
        |> Instruction.Types.PartMsg
        |> dispatch
        |> fun _ ->
            Part.Logic.go2PreviousOrNext instruction part.Title (Instruction.Types.PartMsg >>
                                                                 dispatch) ""

let newModInfo4Debugging newModinfo =
    newModinfo
    |> Seq.map (fun info ->
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
                         ( newModinfo : seq<modificationInfo> ) :
                           ( Data.InstructionData * seq<modificationInfo> ) =
    newModinfo
    |> Seq.map (fun info ->
        currInstruction.Data
        |> Seq.tryFind (fun part ->
            part.Title.Trim() = info.Names.CurrName.Trim())
        |> function
            | res when res.IsSome ->
                Some res.Value
            | _ -> None)
    |> Seq.choose id
    |> fun parts ->
        let newParts4Debugging =
            parts
            |> Seq.map (fun part ->
                part.Title + "\n")
            |> String.concat ""
        { currInstruction with Data = parts}, newModinfo

let updateCurrPositionsTestable (currInstruction : Data.InstructionData)
                        ( currModInfo : seq<modificationInfo> )
                        ( delOrReg : DeleteInfo Option )
                          currName
                        ( newName : Option<string> ) :
                        Data.InstructionData *
                        seq<modificationInfo> =
    ()
    |> function
        | _ when delOrReg.IsSome && newName.IsNone ->
            currModInfo
            |> Seq.map (fun info ->
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
                |> Seq.exists (fun part ->
                    part.Title.Trim() = newName.Value)
            ()
            |> function
                | _ when isNewNameValid = true ->
                    currModInfo
                    |> Seq.map (fun info ->
                        ()
                        |> function
                            | _ when info.Names.CurrName.Trim() = currName.Trim() ->
                                { info with Names = {
                                                CurrName = newName.Value
                                                NewName = None
                                            }}
                            | _ when info.Names.CurrName.Trim() = newName.Value.Trim() ->
                                { info with Names = {
                                                CurrName = currName
                                                NewName = None
                                            }}
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
        |> Seq.tryFind (fun info ->
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
    |> fun x -> seq[x]
    |> Seq.append (
            seq[
                Global.UserPage.Instruction
                |> Global.Page.User
                |> fun page ->
                    (page,None)
                    |> NewPage
            ]
        )
    |> Seq.iter (fun msg -> msg |> dispatch)

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
        |> Seq.map (fun modInfo ->
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

let enableModificationTestable ( modInfoSeq : seq<modificationInfo> Option )
                               ( part : Data.partData )  =
    ()
    |> function
        | _ when modInfoSeq.IsSome ->
            modInfoSeq.Value
            |> Seq.tryFind (fun modInfo ->
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
            
let newNameBecomesCurrent ( modinfo : seq<modificationInfo> ) =
    modinfo
    |> Seq.map (fun info ->
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
                |> Seq.map (fun part ->
                    modInfo
                    |> Seq.tryFind (fun info ->
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
        |> Seq.tryFind (fun info ->
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
        seq[
            Global.divWithStyle
                    (Some "columns is-gapless")
                    ("Instruction Video: " + part.InstructionVideo.Replace("Videos/",""))
                    style

            Global.divWithStyle
                    (Some "columns is-gapless")
                    ("Instruction Text: " + part.InstructionTxt)
                    style
        ]

    seq[
        visible
        Feliz.style.left ( positions.X |> int )
        Feliz.style.top ( positions.Y |> int )
    ]
    |> fun styles ->
        divs,positions,styles

let filenameWStatus (msg : seq<ReactElement> ) =
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

    let getNewModInfos ( modInfos : seq<modificationInfo> ) name =
        modInfos
        |> Seq.map (fun modInfo ->
            modInfo.Status
            |> Seq.map (fun currPartStatus ->
                match currPartStatus with
                | PartStatus.Uploading nameComp ->
                    upDatNameIfMatch name nameComp currPartStatus
                | PartStatus.Delete nameComp ->
                    upDatNameIfMatch name nameComp currPartStatus
                | PartStatus.StatusExisting nameComp ->
                    upDatNameIfMatch name nameComp currPartStatus
                | PartStatus.UploadOrDeleteFinished (nameComp,_) ->
                    upDatNameIfMatch name nameComp currPartStatus)
            |> fun newStatuses ->
                { modInfo with Status = newStatuses })

    let divWSpinner msg =
        seq[
            Html.div[
                prop.className "column"
                prop.children[
                    Fable.React.Helpers.str msg
                ]
            ]
            User.Logic.spinner
        ]

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

    match model.CurrPositions  with
    | Some modInfos ->
        let (newInfo,msgElement) =
            match newStatus with
            | PartStatus.UploadOrDeleteFinished(name,msgElement) ->
                let newModInfos = getNewModInfos modInfos name
                (newModInfos,seq[msgElement])
            | PartStatus.Uploading name ->
                let newModInfos =
                    getNewModInfos modInfos name
                let msgElement =
                    divWSpinner ("Uploading file " + name)
                (newModInfos,msgElement)
            | PartStatus.Delete name ->
                let newModInfos =
                    getNewModInfos modInfos name
                let msgElement =
                    divWSpinner ("Deleting file " + name)
                (newModInfos,msgElement)
            | PartStatus.StatusExisting name ->
                let newModInfos =
                    getNewModInfos modInfos name
                (newModInfos,seq[Html.none])

        let allFileStatusesAreStale =
            modInfos
            |> Seq.collect (fun modInfo ->
                modInfo.Status
                |> Seq.map (fun status ->
                    match status with
                    | PartStatus.UploadOrDeleteFinished(_,_) ->
                        true
                    | _ -> false))
            |> Seq.forall (fun x -> x)
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

let deleteProcess ( status : DeleteProcess<string * Data.Position * Data.DBIds,string * Data.Position * ReactElement> ) =
    match status with
    | DeleteProcess.DeleteInProgress(mediaName,positions,ids) ->
        let mediaToDeleteMsg =
            mediaName
            |> PartStatus.Delete
            |> fun x ->
                (x,positions)
                |> ChangeFileStatus 
                |> User.Types.InstructionMsg
                |> User.Logic.delayedMessage 3000
                |> Cmd.fromAsync

        let nextDeleteProcessMsg =
            ids
            |> User.Logic.deleteAsync mediaName positions
            |> Cmd.fromAsync
            
        seq[
            mediaToDeleteMsg
            nextDeleteProcessMsg
        ]
    | DeleteProcess.DeleteFinished(mediaName,positions,reactEL) ->
        let mediaToDeleteMsg =
            (mediaName,reactEL)
            |> PartStatus.UploadOrDeleteFinished 
            |> fun x ->
                (x, positions)
                |> ChangeFileStatus 
                |> User.Types.InstructionMsg
                |> Cmd.ofMsg
        mediaToDeleteMsg
        |> fun x -> seq[x]
        
        
let uploadOrDeleteFinished modInfosOpt =
    match modInfosOpt with
    | Some modInfos ->
        modInfos
        |> Seq.collect (fun modInfo ->
            modInfo.Status
            |> Seq.map (fun status ->
                match status with
                | PartStatus.UploadOrDeleteFinished(_,_) ->
                    true
                | _ -> false))
        |> Seq.forall (fun res -> res)
        |> function
            | uploadOrDeleteFinished when uploadOrDeleteFinished = true ->
                let modInfosNew =
                    modInfos
                    |> Seq.map (fun modInfo ->
                        modInfo.Status
                        |> Seq.map (fun status ->
                            match status with
                            | PartStatus.UploadOrDeleteFinished(name,_) ->
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
                Some modInfosNew
            | _ -> None
    | _ -> None

let databaseChangeProcedure  ( status : DatabaseChangeProcess<seq<Data.DatabaseSavingOptions> * Data.DBIds * Data.Position,
                                                              ReactElement * Data.Position> ) =
    match status with
    | DatabaseChangeBegun(dbSaveOpt,ids,positions) ->
        let databaseChangesMsg =
            "Performing database changes..."

        let divWSpinner =
            seq[
                Html.div[
                    prop.className "column"
                    prop.children[
                        Fable.React.Helpers.str databaseChangesMsg
                    ]
                ]
                User.Logic.spinner
            ]
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
            dbSaveOpt
            |> User.Logic.saveInstructionToDatabase ids positions

        dbChangeMsg
        |> Seq.append (seq[popupMsg])

    | DatabseChangeFinished(msg,positions) ->
        let funcChaining positions msg =
            (seq[msg],positions) |>
            (
                PopUpSettings.DefaultWithButton >>
                Some >>
                User.Types.PopUpMsg >>
                Cmd.ofMsg
            )

        let databaseChangePopupMsg =
            msg
            |> funcChaining positions

        seq[databaseChangePopupMsg]
        
