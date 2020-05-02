module NewAdd.Logic


open Feliz
open Browser
open Fable.Core.JsInterop
open Elmish
open User.Types

let createNewInstructionSequence ( usrData : Data.UserData ) =
    usrData.Instructions
    |> Seq.map (fun instruction -> instruction.Title)
    |> ( NewAdd.Types.NewInstructionsListMsg >> User.Types.NewAddMsg)
    |> fun x -> seq[x]
    |> Seq.map ( fun msg -> msg |> Cmd.ofMsg )

let newInstructionSelected ( ev : Types.Event ) dispatch =
    let instrName = ev.target?value |> string
    Some instrName
    |> ( User.Types.NewAddNewCurrentInstruction >> dispatch)

let saveNewData newInstrDataOpt
              ( newInstr : Data.InstructionData )
                dbIds
                positions =
    match newInstrDataOpt with
    | Some medias ->
        let matchMaking str =
            medias
            |> Seq.exists (fun media ->
                match media with
                | NewAdd.Types.Video vid ->
                    vid.name = str
                | NewAdd.Types.InstructionTxt instr ->
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
                    match media with
                    | NewAdd.Types.MediaChoiceFormData.Video file -> file
                    | NewAdd.Types.MediaChoiceFormData.InstructionTxt file -> file)
                |> Seq.map (fun file ->
                    (file,dbIds,positions)
                    |> funcChaining
                    |> Cmd.ofMsg)
                |> Cmd.batch
                |> fun msg ->
                    msg
            | _ ->
                "Not all necesarry media exists!"
                |> User.Logic.errorPopupMsg positions
                |> Cmd.ofMsg
                |> fun msg ->
                    msg
    | None -> []


