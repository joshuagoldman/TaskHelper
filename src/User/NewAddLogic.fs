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

let modifyFileData ( newInstrData : seq<NewAdd.Types.MediaChoiceFormData> Option )
                   ( instr : Data.InstructionData Option )
                     id =
    newInstrData
    |> function
        | res when instr.IsSome && res.IsSome ->
            res.Value
            |> Seq.map (fun media ->
                match media with
                | NewAdd.Types.Video (file,isUploading, _) ->
                    (instr.Value,id)
                    |> NewAdd.Types.Add
                    |> fun ``type`` ->
                        (file,isUploading, ``type``)
                        |> NewAdd.Types.Video
                | NewAdd.Types.InstructionTxt (file,isUploading,``type``) ->
                    (instr.Value,id)
                    |> NewAdd.Types.Add
                    |> fun ``type`` ->
                        (file,isUploading, ``type``)
                        |> NewAdd.Types.InstructionTxt)
            |> Some
        | res -> res 

