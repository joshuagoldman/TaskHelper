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
    | CreateNewDataMsg(status) ->
        let msg =
            Logic.saveUserData status
        model,Cmd.batch(msg)    
    | NewAddInfoMsg reactMessage ->
        { model with  NewAddMessages = reactMessage }, []
    | NewFilesChosenMsg (files,type') ->
        { model with NewInstructionData =
                        (User.Logic.extractMedia model.NewInstructionData files type' |> Some)}, []
    | NewInstructionsListMsg sequence ->
        { model with InstructionList = Some sequence }, Cmd.none
    | NewCurrentInstructionMsg instrWId ->
        { model with CurrentInstruction = instrWId }, Cmd.none
    | SaveNewData (newInstr,dbIds,positions) ->
        let msg =
            Instruction.Logic.saveNewData model.NewInstructionData
                                     newInstr
                                     dbIds
                                     positions
        model,msg

        

       
