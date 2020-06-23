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

let init () : Model * Cmd<Msg<'a>> =
    {
       NewInstructionData =  None
       NewAddMessages = [|str ""|]
       LoadIcon = defaultAppearanceAttributes
       VideosUploadInput = defaultAppearanceAttributes
       InstructionTxtUploadInput = defaultAppearanceAttributes
       InstructionList = None
       CurrentInstruction = None
    }, []

let update msg model : NewAdd.Types.Model * Cmd<User.Types.Msg>  =
    match msg with
    | CreateNewDataMsg(status) ->
        let msg =
            Logic.saveUserData status
        model,Cmd.batch(msg)    
    | NewAddInfoMsg reactMessage ->
        match model.CurrentInstruction with
        | Some (_,id) ->
            { model with  NewAddMessages = reactMessage
                          CurrentInstruction = (None,id) |> Some}, []
        | None ->
        { model with  NewAddMessages = reactMessage }, []
    | NewFilesChosenMsg (files,type') ->
        { model with NewInstructionData =
                        (User.Logic.extractMedia model.NewInstructionData files type' |> Some)}, []
    | NewInstructionsListMsg sequence ->
        { model with InstructionList = Some sequence }, Cmd.none
    | NewCurrentInstructionMsg instrWId ->
        console.log(instrWId)
        { model with CurrentInstruction = instrWId }, Cmd.none
    | NewDataToInstruction(newInstr,dbIds,utils) ->
        match model.NewInstructionData with
        | Some instrData ->
            let createInstructionMsg =
                (newInstr,dbIds,utils,instrData)
                |> User.Types.SaveNewData
                |> Cmd.ofMsg
            model,createInstructionMsg
        | _ -> model,[]
           

        

       
