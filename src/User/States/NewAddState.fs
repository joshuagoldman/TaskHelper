module NewAdd.State

open Elmish
open Controls
open Types
open Part
open Data
open NewAdd.Types

let init () : Model * Cmd<Msg> =
    {
       NewInstructionData = Error "No Instruction files have been loaded..."
       NewAddMessages = ""
       LoadIcon = defaultAppearanceAttributes
    }, []


let update msg model : NewAdd.Types.Model * Cmd<Msg>  =
    match msg with
    | CreateNewDataMsg Started ->
        { model with NewInstructionData = InProgress }, Cmd.batch
                                                    (Logic.getUserDataUpdate InProgress
                                                    |> Seq.map (fun msg -> Cmd.ofMsg msg)
                                                    |> Seq.append [Cmd.fromAsync (Logic.loadInstructionItems model)])
                                                    
    | CreateNewDataMsg (Finished (Error error)) ->
        { model with NewInstructionData = Resolved ( Error error)}, Cmd.batch
                                                                (Logic.getUserDataUpdate
                                                                            (Resolved ( Error error))
                                                                |> Seq.map (fun msg -> Cmd.ofMsg msg))
    | CreateNewDataMsg (Finished (Ok items)) ->
        { model with NewInstructionData = Resolved ( Ok items)}, Cmd.batch
                                                            (Logic.getUserDataUpdate (Resolved ( Ok items))
                                                            |> Seq.map (fun msg -> Cmd.ofMsg msg)
                                                            |> Seq.append [Cmd.ofMsg LoginSuceeded])
    | NewAddMsg str ->
        { model with  NewAddMessages = str }, []
