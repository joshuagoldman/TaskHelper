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


let updateCurrPositions model (
                                ( posOpt : int Option),
                                ( checkedOpt : bool Option),
                                  namePair
                               ) =
    let defaultValueToReturn = model, []

    let currentPositionsValues =
        model.CurrPositions.Value
        |> Seq.map (fun modinfo ->
            modinfo.Names.CurrName + ", " + ( if modinfo.Names.NewName.IsSome
                                              then modinfo.Names.NewName.Value
                                              else "null") + ",  ")
        |> String.concat ""
    
    ()
    |> function
        | _ when model.CurrPositions.IsSome ->
            let currPositions = model.CurrPositions.Value
    
            let currPartPositionOpt =
                Seq.zip currPositions (Global.getPositionSequence currPositions)
                |> Seq.tryFind (fun (modInfo,_) ->
                    modInfo.Names.CurrName = namePair.CurrName)
                |> function
                    | res when res.IsSome ->
                        res.Value |> fun (_,pos) ->
                            Some pos
                    | _ -> None
    
            let newPositionsPositionModified =
                ()
                |> function
                    | _ when posOpt.IsSome && currPartPositionOpt.IsSome ->
                        Seq.zip currPositions (Global.getPositionSequence currPositions)
                        |> Seq.map (fun (modInfo,currPos) ->
                            ()
                            |> function
                                | _ when currPos = currPartPositionOpt.Value ->
                                    {
                                        Names = modInfo.Names
                                        Position = posOpt
                                        IsChecked = modInfo.IsChecked
                                    }
                                | _ when currPos = posOpt.Value ->
                                    {
                                        Names = modInfo.Names
                                        Position = currPartPositionOpt
                                        IsChecked = modInfo.IsChecked
                                    }
                                | _ -> modInfo)
                    | _ -> currPositions
    
            let newPositionsNameModified =
                ()
                |> function
                    | _ when namePair.NewName.IsSome && currPartPositionOpt.IsSome ->
                        Seq.zip currPositions (Global.getPositionSequence currPositions)
                        |> Seq.map (fun (modInfo,currPos) ->
                            ()
                            |> function
                                | _ when currPos = currPartPositionOpt.Value ->
                                    {
                                        Names = {
                                            CurrName = modInfo.Names.CurrName
                                            NewName = namePair.NewName
                                        }
                                        Position = modInfo.Position
                                        IsChecked = modInfo.IsChecked
                                    }
                                | _ -> modInfo)
                    | _ -> newPositionsPositionModified
    
            let newPositionsCheckedModified =
                ()
                |> function
                    | _ when checkedOpt.IsSome && currPartPositionOpt.IsSome ->
                        Seq.zip currPositions (Global.getPositionSequence currPositions)
                        |> Seq.map (fun (modInfo,currPos) ->
                                ()
                                |> function
                                    | _ when currPos = currPartPositionOpt.Value ->
                                        {
                                            Names = modInfo.Names
                                            Position = modInfo.Position
                                            IsChecked = checkedOpt
                                        }
                                    | _ -> modInfo)
                    | _ -> newPositionsNameModified
    
            { model with CurrPositions = Some newPositionsCheckedModified }, []
    
        | _ -> defaultValueToReturn

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

