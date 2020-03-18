module NewAdd.Logic


open Feliz
open Browser
open Fable.Core.JsInterop
open Elmish

let createNewInstructionSequence ( usrData : Data.UserData ) =
    let initInstructionMsg =
        usrData.Instructions
        |> Seq.item 0
        |> fun instrction -> instrction.Title
        |> ( Some >> User.Types.NewAddNewCurrentInstruction )
    usrData.Instructions
    |> Seq.map (fun instruction -> instruction.Title)
    |> ( NewAdd.Types.NewInstructionsListMsg >> User.Types.NewAddMsg)
    |> fun x ->
        seq[
            x
            initInstructionMsg
        ]
    |> Seq.map ( fun msg -> msg |> Cmd.ofMsg )

let newInstructionSelected ( ev : Types.Event ) dispatch =
    let instrName = ev.target?value |> string
    Some instrName
    |> ( User.Types.NewAddNewCurrentInstruction >> dispatch)


