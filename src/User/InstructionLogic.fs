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

let updateCurrPositionsTestable (currInstruction : Data.InstructionData)
                                (currModInfo : seq<modificationInfo>)
                                (delOrReg : DeleteInfo Option)
                                (namePair : NamePair) :
                                Data.InstructionData *
                                seq<modificationInfo> =
                
            
        ()
        |> function
            | _ when delOrReg.IsSome && namePair.NewName.IsNone ->
                currModInfo
                |> Seq.map (fun info ->
                    info.Names.CurrName
                    |> function
                        | name when name = namePair.CurrName ->
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
                |> Seq.filter (fun info ->
                    match info.DelOrReg.Value with
                    | Delete _ ->
                        true
                    | Regret _ ->
                        false)
                |> fun newModInfo ->
                    newModInfo
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
                        { currInstruction with Data = parts}, newModInfo
            | _ when namePair.NewName.IsSome && delOrReg.IsNone ->
                currModInfo
                |> Seq.map (fun info ->
                    ()
                    |> function
                        | _ when info.Names.CurrName = namePair.CurrName ->
                            { info with Names = {
                                            CurrName = namePair.NewName.Value
                                            NewName = None
                                        }}
                        | _ when info.Names.CurrName = namePair.NewName.Value ->
                            { info with Names = {
                                            CurrName = namePair.CurrName
                                            NewName = None
                                        }}
                        | _ -> info
                    )
                    
                |>  fun newModinfo ->
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
            (res.Value.DelOrReg, res.Value.Names)
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
    {
        CurrName = partName
        NewName = Some newPartName 
    }
    |> fun namePair ->
        (None,namePair)
        |> (Instruction.Types.NewModificationInfo >> dispatch)

