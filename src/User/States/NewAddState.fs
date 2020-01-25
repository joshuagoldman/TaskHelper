module NewAdd.State

open Elmish
open Controls
open Types
open Part
open Data
open NewAdd.Types
open User

let init () : Model * Cmd<Msg> =
    {
       NewInstructionData = Deferred.HasNostStartedYet
       NewAddMessages = ""
       LoadIcon = defaultAppearanceAttributes
    }, []


let update msg model : NewAdd.Types.Model * Cmd<Msg>  =
    match msg with
    | CreateNewDataMsg Started ->
        { model with NewInstructionData = InProgress },
         Cmd.batch( User.Logic.saveNewInstruction InProgress)
                                                    
    | CreateNewDataMsg (Finished (Error error)) ->
        { model with NewInstructionData = Resolved ( Error error)},
         Cmd.batch( User.Logic.saveNewInstruction (Resolved (Error error)))
    | CreateNewDataMsg (Finished (Ok items)) ->
        { model with NewInstructionData = Resolved ( Ok items)},
         Cmd.batch( User.Logic.saveNewInstruction (Resolved (Ok items)))
    | NewAddInfoMsg str ->
        { model with  NewAddMessages = str }, []
