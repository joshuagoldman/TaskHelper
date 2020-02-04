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
       NewInstructionData =  None
       NewAddMessages = ""
       LoadIcon = defaultAppearanceAttributes
       NewInstructionId = None
       VideosUploadInput = defaultAppearanceAttributes
       InstructionTxtUploadInput = defaultAppearanceAttributes
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
    | NewFilesChosenMsg (files,type') ->
        { model with NewInstructionData =
                        (User.Logic.extractMedia model.NewInstructionData files type' |> Some)}, []
