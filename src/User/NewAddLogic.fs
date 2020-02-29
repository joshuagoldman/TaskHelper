module NewAdd.Logic


open Feliz
open Browser
open Fable.Core.JsInterop

let createNewInstructionSequence ( usrData : Data.UserData ) =
    usrData.Instructions
    |> Seq.map (fun instruction -> instruction.Title)
    |> ( NewAdd.Types.NewInstructionsListMsg >> User.Types.NewAddMsg)

let newInstructionSelected ( ev : Types.Event ) dispatch =
    let instrName = ev.target?value |> string
    instrName
    |> ( User.Types.NewAddNewCurrentInstruction >> dispatch)

let modifyFileData ( newInstrData : seq<NewAdd.Types.MediaChoiceFormData> Option )
                   ( instr : Data.InstructionData Option ) =
    newInstrData
    |> function
        | res when instr.IsSome && res.IsSome ->
            res.Value
            |> Seq.map (fun media ->
                match media with
                | NewAdd.Types.Video (file,isUploading, _) ->
                    instr.Value
                    |> NewAdd.Types.Add
                    |> fun ``type`` ->
                        (file,isUploading, ``type``)
                        |> NewAdd.Types.Video
                | NewAdd.Types.InstructionTxt (file,isUploading,``type``) ->
                    instr.Value
                    |> NewAdd.Types.Add
                    |> fun ``type`` ->
                        (file,isUploading, ``type``)
                        |> NewAdd.Types.InstructionTxt)
            |> Some
        | res -> res 

