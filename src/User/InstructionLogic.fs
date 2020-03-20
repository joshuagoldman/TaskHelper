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
            User.Types.Position.X = ( ev?pageX : float )
            User.Types.Position.Y = ( ev?pageY : float )
        }

    let divs =
        seq[
            Global.divWithStyle
                    None
                    ("Instruction Video:" + part.InstructionVideo)
                    (prop.style[style.fontSize 10])

            Global.divWithStyle
                    None
                    ("Instruction Text:" + part.InstructionTxt)
                    (prop.style[style.fontSize 10])
        ]

    seq[
        visible
        style.left ( positions.X |> int )
        style.top ( positions.Y |> int )
    ]
    |> fun styles ->
        divs,positions,styles
