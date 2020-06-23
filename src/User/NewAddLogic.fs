module NewAdd.Logic


open Feliz
open Browser
open Fable.Core.JsInterop
open Elmish
open User.Types

let createNewInstructionSequence ( usrData : Data.UserData ) =
    usrData.Instructions
    |> Array.map (fun instruction ->
        match instruction.Title with
        | Data.InstructionTitleInfo.HasOldName title ->
            title
        | Data.InstructionTitleInfo.HasNewName titles ->
            titles.DbName
        )
    |> ( NewAdd.Types.NewInstructionsListMsg >> User.Types.NewAddMsg)
    |> fun x -> [|x|]
    |> Array.map ( fun msg -> msg |> Cmd.ofMsg )

let newInstructionSelected ( ev : Types.Event ) dispatch =
    let instrName = ev.target?value |> string
    Some instrName
    |> ( User.Types.NewAddNewCurrentInstruction >> dispatch)


