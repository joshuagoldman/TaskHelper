module Instruction.Logic

open Fable.Core
open Fable.React
open Fable.React.Props
open Instruction.Types
open Feliz
open Browser
open Fable.Core.JsInterop

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

let ajustAfterNewModinfo ( currInstruction : Data.InstructionData )
                         ( newModinfo : seq<modificationInfo> ) :
                           ( Data.InstructionData * seq<modificationInfo> ) =
    newModinfo
    |> Seq.map (fun info ->
        currInstruction.Data
        |> Seq.tryFind (fun part ->
            part.Title = info.Names.CurrName)
        |> function
            | res when res.IsSome ->
                Some res.Value
            | _ -> None)
    |> Seq.choose id
    |> fun parts ->
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
            currModInfo
            |> Seq.map (fun info ->
                ()
                |> function
                    | _ when info.Names.CurrName = currName ->
                        { info with Names = {
                                        CurrName = newName.Value
                                        NewName = None
                                    }}
                    | _ when info.Names.CurrName = newName.Value ->
                        { info with Names = {
                                        CurrName = currName
                                        NewName = None
                                    }}
                    | _ -> info
                )
                
            |>  fun newModinfo ->
                newModinfo
                |> ajustAfterNewModinfo currInstruction
        | _ -> currInstruction, currModInfo

let getCurrentDelOrReg model ( part : Data.partData ) =
    match model.CurrPositions with
    | Some modinfo ->
        modinfo
        |> Seq.tryFind (fun info ->
            info.Names.CurrName = part.Title)
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
                            strVal
                        | Regret strVal ->
                            strVal
                    | _ -> ""
            | _ -> ""

let newPartSelected ( ev : Types.Event ) partName dispatch =
    let newPartName = ev.target?value |> string

    (None,partName,Some newPartName)
    |> (Instruction.Types.NewModificationInfo >> dispatch)

let partNameToChange dispatch
    ( part : Data.partData )
    newPartName =
        if newPartName |> String.length < 30
        then true
        else false
        |> function
            | res when res = true ->
                (part.Title,newPartName)
                |> (Instruction.Types.UpdateNewName >> dispatch)
            | _ -> ()
let updateNewNameTestable currPositions currName newName=
    currPositions
    |> Seq.map (fun modInfo ->
        ()
        |> function
            | _ when modInfo.Names.CurrName = currName ->
                { modInfo.Names with CurrName = newName}
                |> fun newModInfo ->
                    { modInfo with Names = newModInfo }
            | _  -> modInfo)

let modifyNames model dispatch =
    match model.CurrPositions with
     | Some currPositions ->
        currPositions
        |> Seq.iter (fun modInfo ->
            ()
            |> function
                | _ when modInfo.Names.NewName.IsSome ->
                    (modInfo.Names.CurrName,modInfo.Names.NewName.Value)
                    |> (Instruction.Types.NewName >> dispatch)
                | _ -> ())
     | _ -> ()
            
    
