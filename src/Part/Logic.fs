module Part.Logic

open State
open Part.Types
open Fable.Core
open Browser

let go2PreviousOrNext ( instruction : Data.InstructionData )
                        partTitle
                        dispatch
                        buttonName =


    let partSequence =
        instruction.Data
        |> fun sequence -> Seq.zip sequence [0..sequence |> Seq.length |> fun x -> x-1]

    let nextOrPrevious pos =
        let nextPrevious =
            match buttonName with
            | "NextButton" -> 1
            | "PreviousButton" -> -1
            | _ -> 0

        partSequence
        |> Seq.item (pos + nextPrevious)
        |> fun (data,_) ->
              (data,instruction)
              |> NewPart2Show
              |> dispatch
              |> fun _ -> (true, buttonName)
                          |> MakeButtonVisible
                          |> dispatch

    let partPositionChoices pos =
        partSequence
        |> Seq.length
        |> fun x -> x - 1
        |> function
            | lastPos when pos = lastPos - 1 && buttonName = "NextButton" ->
                partSequence
                |> Seq.item lastPos
                |> fun (data,_) ->
                      (data,instruction)
                      |> NewPart2Show
                      |> dispatch
                      |> fun _ -> (false, buttonName)
                                  |> MakeButtonVisible
                                  |> dispatch

            | lastPos when pos = lastPos && buttonName = ""  ->
                (false, "NextButton")
                |> MakeButtonVisible
                |> dispatch
                

            | _ when pos = 1 && buttonName = "PreviousButton"  ->
                partSequence
                |> Seq.item 0
                |> fun (data,_) ->
                      (data,instruction)
                      |> NewPart2Show
                      |> dispatch
                      |>fun _ -> (false, buttonName)
                                 |> MakeButtonVisible
                                 |> dispatch

             | _ when pos = 0 && buttonName = "" ->
                (false, "PreviousButton")
                |> MakeButtonVisible
                |> dispatch

             | _ -> nextOrPrevious pos

    let result = 
        partSequence
        |> Seq.tryFind (fun (data,_) -> partTitle = data.Title)
                        |> function
                            | res when res = None -> ()
                            | res ->  res.Value
                                        |> fun (_,pos) ->
                                                partPositionChoices pos

    result



let whichNavigationButton model buttonName =
    match buttonName with
    | "PreviousButton" -> model.PreviousButton
    | "NextButton" -> model.NextButton
    | _ -> model.PreviousButton

let checkInstructionAvailability model dispatch buttonName =
    match model.Instruction with
    | Ok instruction ->
        match model.Data with
        | Ok part ->
            go2PreviousOrNext instruction (part.Title) dispatch buttonName
        | Error err -> err |> (Part.Types.SendErrorMessage >> dispatch)
    | Error err ->
        err |> (Part.Types.SendErrorMessage >> dispatch)

