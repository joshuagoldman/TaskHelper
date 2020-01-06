module User.Logic

open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Feliz
open Types
open State
open System
open Data
open InstructionSearch.Types

let go2PartOrInstruction dispatch result =
    match result with
    | Part (partModel, _) ->
        Part.State.NewPart2Show partModel
        |> Instruction.State.PartMsg
        |> User.Types.InstructionMsg
        |> dispatch
        |> fun _ -> Part.Logic.go2PreviousOrNext partModel (Instruction.State.PartMsg >>
                                                            User.Types.InstructionMsg >>
                                                            dispatch) "" 
    | Instruction (instructionModel, _) ->
        Instruction.State.NewInstruction2Show instructionModel
        |> User.Types.InstructionMsg
        |> dispatch

let WritePartOrInstruction result =
    match result with
    | Part (instruction, _) -> instruction.Title
    | Instruction (data, _) -> data.Title

let choosePage page =
    match page with
    | Part (_,_) -> Global.Part
    | Instruction (_,_) -> Global.Instruction

let searchInfo info (keyWord : string) =
    match info with
    | Instruction (model, _) -> model.Title.ToLower().Contains keyWord && keyWord <> ""
    | Part (model, _) -> model.Title.ToLower().Contains keyWord && keyWord <> ""

let instructionResults =
    allData ""
    |> Seq.map (fun instruction -> Instruction(instruction, []))
let partResults =
    allData ""
    |> Seq.collect (fun instruction -> instruction.Data
                                       |> Seq.map (fun part -> Part(part, [])))

