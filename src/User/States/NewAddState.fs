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
       NewInstructionData = Error ""
       NewAddMessages = ""
       LoadIcon = defaultAppearanceAttributes
       NewInstructionId = None
    }, []


let update msg model : NewAdd.Types.Model * Cmd<User.Types.Msg>  =
    match msg with
    | CreateNewDataMsg ( Ok data ) ->
        model,
        Cmd.batch(
           Logic.saveUserData data 
        )
    | NewAddInfoMsg str ->
        { model with  NewAddMessages = str }, []
    | NewInstructionIdMsg str ->
        { model with NewInstructionId = Some str}, []
    | PostInstruction files ->
        model, Logic.createInstructionFromFile files model.NewInstructionId
