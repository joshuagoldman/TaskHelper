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
       Progressbar =
        { defaultAppearanceAttributes with Visible = style.visibility.hidden } 
    }, []


let update msg model : NewAdd.Types.Model * Cmd<User.Types.Msg>  =
    match msg with
    | CreateNewDataMsg ( Ok data ) ->
        model,
        Cmd.batch(
           style.visibility.visible |>
           ( NewAdd.Types.ProgressBarVisibleMsg >>
             User.Types.NewAddMsg )
           |> Cmd.ofMsg
           |> fun x -> seq[x]
           |> Seq.append (Logic.saveUserData data )
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
    | ProgressBarVisibleMsg visibility ->
        let newVisibility =
            visibility
            |> function
               | res when res = style.visibility.visible ->
                   style.visibility.hidden
               | _ ->
                   style.visibility.visible 
            
        { model with Progressbar =
                        { model.Progressbar with Visible = newVisibility } }, []
