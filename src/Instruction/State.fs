module Instruction.State

open Elmish
open Controls
open Types
open Part
open Data

type Msg =
    | NewInstruction2Show of Data.InstructionData

let init () : Model * Cmd<Msg> =

    {
      Instruction = allData |> Seq.item 0 
    }, []


let update msg model : Instruction.Types.Model * Cmd<Msg>  =
    match msg with
    | NewInstruction2Show instruction -> { model with Instruction = instruction }, []
