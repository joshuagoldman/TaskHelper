module Instruction.Logic

open Fable.Core
open Fable.React
open Fable.React.Props
open Instruction.Types
open Feliz
open Browser


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

let upDatePosition ( part : Data.partData )
                   ( dispatch : Msg -> unit)
                   ( str : string ) =
    match System.Int32.TryParse(str) with
    | true, num ->
        let namePair = {
            CurrName = part.Title
            NewName = None
        }
        (Some num, None, namePair)
        |> ( Instruction.Types.NewModificationInfo >> dispatch )
    | _ -> ()

let upDateName ( part : Data.partData )
               ( dispatch : Msg -> unit)
               ( str : string ) =
    str
    |>function
        | _ when str |> String.length < 15 ->
            let namePair = {
                CurrName = part.Title
                NewName = Some str
            }
            (None, None, namePair)
            |> ( Instruction.Types.NewModificationInfo >> dispatch )
            
        | _ -> ()

let upDateChecked ( part : Data.partData )
                  ( dispatch : Msg -> unit)
                  ( ``checked`` : bool ) =

    let namePair = {
        CurrName = part.Title
        NewName = None
    }
    (None, Some ``checked``, namePair)
    |> ( Instruction.Types.NewModificationInfo >> dispatch )

let startSaving model dispatch =
    match model.CurrInstruction with
    | Ok instruction ->
        ()
        |>function
          | _ when model.CurrPositions.IsSome ->
              (instruction.Data, model.CurrPositions.Value)
              |>function
                 | (parts,newData) when parts |> Seq.length <> 0 &&
                                        newData |> Seq.length <> 0 ->
                        parts
                        |> Seq.map (fun part ->
                                newData
                                |> Seq.tryFind (fun newPart -> newPart.Names.CurrName = part.Title)
                                |> function
                                    | res when res.IsSome ->
                                        let newPartInfo = res.Value
                                        ()
                                        |> function
                                            | _ when newPartInfo.Position.IsSome ->
                                                parts
                                                |> Seq.item newPartInfo.Position.Value
                                                |> fun x ->
                                                    { x with Title =
                                                             if newPartInfo.Names.NewName.IsSome
                                                             then newPartInfo.Names.NewName.Value
                                                             else x.Title}
                                            | _ -> part
                                    | _ -> part
                           )
                        |> fun partData ->
                            {
                                Data.InstructionData.Data = partData
                                Data.InstructionData.Title = instruction.Title
                            }
                        |> Instruction.Types.NewInstruction2Show
                        |> dispatch
                    
                 | _ -> ()
          | _ -> ()
    | Error _ -> ()

