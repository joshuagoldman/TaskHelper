module NewAdd.Logic


open Feliz
open Browser

let createNewInstructionSequence ( usrData : Data.UserData ) =
    usrData.Instructions
    |> Seq.map (fun instruction -> instruction.Title)
    |> ( NewAdd.Types.NewInstructionsListMsg >> User.Types.NewAddMsg)

