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
       VideosUploadInput = defaultAppearanceAttributes
       InstructionTxtUploadInput = defaultAppearanceAttributes
       InstructionList = None
       CurrentInstructionWId = None
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
    | PostInstruction files ->
        model, []
    | NewFilesChosenMsg (files,type') ->
        { model with NewInstructionData =
                        (User.Logic.extractMedia model.NewInstructionData files type' |> Some)}, []
    | ChangeFileStatus (media,newStatus) ->
        Logic.changeFileStatus model media newStatus
    | NewInstructionsListMsg sequence ->
        { model with InstructionList = Some sequence }, Cmd.none
    | NewCurrentInstructionMsg (instr,id) ->
        let newInstructionData =
            NewAdd.Logic.modifyFileData model.NewInstructionData instr id
        { model with CurrentInstructionWId = Some (instr,id)
                     NewInstructionData = newInstructionData}, Cmd.none

       
