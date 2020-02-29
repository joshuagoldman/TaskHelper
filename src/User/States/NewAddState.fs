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
open Fable.Core
open Browser

let init () : Model * Cmd<Msg> =
    {
       NewInstructionData =  None
       NewAddMessages = seq[str ""]
       LoadIcon = defaultAppearanceAttributes
       NewInstructionId = None
       VideosUploadInput = defaultAppearanceAttributes
       InstructionTxtUploadInput = defaultAppearanceAttributes
       InstructionList = None
       CurrentInstruction = None
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
                |> Seq.append(
                    seq[
                        (model.NewInstructionData |>
                         ( SavingResolved >>
                           SavingFinished >>
                           NewAdd.Types.CreateNewDataMsg >>
                           User.Types.NewAddMsg))
                        |> Cmd.ofMsg
                     ]
                )
                    
            )
    | CreateNewDataMsg(SavingFinished(status)) ->
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
    | NewInstructionsListMsg sequence ->
        { model with InstructionList = Some sequence }, Cmd.none
    | NewCurrentInstructionMsg instr ->
        let newInstructionData =
            NewAdd.Logic.modifyFileData model.NewInstructionData instr
        { model with CurrentInstruction = instr
                     NewInstructionData = newInstructionData}, Cmd.none

       
