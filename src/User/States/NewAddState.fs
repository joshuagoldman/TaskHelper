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
    | PostInstruction files ->
        model, []
    | NewFilesChosenMsg (files,type') ->
        { model with NewInstructionData =
                        (User.Logic.extractMedia model.NewInstructionData files type' |> Some)}, []
    | ChangeFileStatus (media) ->
        Logic.changeFileStatus model media
    | NewInstructionsListMsg sequence ->
        { model with InstructionList = Some sequence }, Cmd.none
    | NewCurrentInstructionMsg instrWId ->
        { model with CurrentInstruction = instrWId }, Cmd.none
    | SaveNewData (newInstr,dbIds,positions) ->
        
        match model.NewInstructionData with
        | Some medias ->
            let matchMaking str =
                medias
                |> Seq.exists (fun media ->
                    match media with
                    | NewAdd.Types.Video (vid,_) ->
                        vid.name = str
                    | NewAdd.Types.InstructionTxt (instr,_) ->
                        instr.name = str)
            newInstr.Data
            |> Seq.forall (fun part ->
                matchMaking part.InstructionTxt &&
                matchMaking part.InstructionVideo)
            |> function
                | res when res = true ->
                    let funcChaining info =
                        info |>
                        (
                            Data.SavingHasNostStartedYet >>
                            Data.SavingOnGoing >>
                            NewAdd.Types.CreateNewDataMsg >>
                            User.Types.NewAddMsg
                        )
                    medias
                    |> Seq.map (fun media ->
                        (media,dbIds,positions)
                        |> funcChaining
                        |> Cmd.ofMsg)
                    |> Cmd.batch
                    |> fun msg ->
                        model,msg
                | _ ->
                    "Not all necesarry media exists!"
                    |> Logic.errorPopupMsg positions
                    |> Cmd.ofMsg
                    |> fun msg ->
                        model,msg
        | None -> model,[]
    | _ -> model,[]

       
