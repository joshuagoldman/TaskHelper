module NewAdd.Logic


open Feliz
open Browser
open Fable.Core.JsInterop
open Elmish
open User.Types

let createNewInstructionSequence ( usrData : Data.UserData ) =
    usrData.Instructions
    |> Array.map (fun instruction -> instruction.Title)
    |> ( NewAdd.Types.NewInstructionsListMsg >> User.Types.NewAddMsg)
    |> fun x -> [|x|]
    |> Array.map ( fun msg -> msg |> Cmd.ofMsg )

let newInstructionSelected ( ev : Types.Event ) dispatch =
    let instrName = ev.target?value |> string
    Some instrName
    |> ( User.Types.NewAddNewCurrentInstruction >> dispatch)


