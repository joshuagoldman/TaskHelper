module NewAdd.State

open Elmish
open Controls
open Types
open Part
open Data
open NewAdd.Types
open User
open Fable.React
open Feliz

let init () : Model * Cmd<Msg> =
    {
       NewInstructionData =  None
       NewAddMessages = seq[str ""]
       LoadIcon = defaultAppearanceAttributes
       NewInstructionId = None
       VideosUploadInput = defaultAppearanceAttributes
       InstructionTxtUploadInput = defaultAppearanceAttributes
    }, []

let update msg model : NewAdd.Types.Model * Cmd<User.Types.Msg>  =
    match msg with
    | CreateNewDataMsg(SavingWillBegin(status)) ->
        model,
            Cmd.batch(
                Logic.saveUserData status
            )    

    | CreateNewDataMsg(SavingOnGoing(status)) ->
        model,
            Cmd.batch(
                Logic.saveUserData status
            )    

    | NewAddInfoMsg reactMessage ->
        { model with  NewAddMessages = reactMessage }, []
    | NewInstructionIdMsg str ->
        { model with NewInstructionId = Some str}, []
    | PostInstruction files ->
        model, Logic.createInstructionFromFile files model.NewInstructionId
    | NewFilesChosenMsg (files,type') ->
        { model with NewInstructionData =
                        (User.Logic.extractMedia model.NewInstructionData files type' |> Some)}, []
    | ChangeFileStatus (media,newStatus) ->
        Logic.changeFileStatus model media newStatus

       
