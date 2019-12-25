module Part.Logic

open State

let go2PreviousOrNext ( model : Part.Types.Model ) dispatch buttonName plusMinus =

    let partSequence =
        Data.allData
        |> Seq.collect (fun instruction -> instruction.Data
                                           |> Seq.map (fun dt -> dt))
        |> fun sequence -> Seq.zip sequence [0..sequence |> Seq.length |> fun x -> x-1]


    let partPositionChoices pos =
        partSequence
        |> Seq.length
        |> fun x -> x - 1
        |> function
            | lastPos when pos = lastPos - 1 && plusMinus = 1 ->
                partSequence
                |> Seq.item lastPos
                |> fun (data,_) ->
                      data
                      |> NewPart2Show
                      |> dispatch
                      |> fun _ -> (false, buttonName)
                                  |> MakeButtonVisible
                                  |> dispatch

            | lastPos when pos = lastPos && plusMinus = 1 -> ()
                

            | _ when pos = 1 && plusMinus = -1  ->
                partSequence
                |> Seq.item 0
                |> fun (data,_) ->
                      data
                      |> NewPart2Show
                      |> dispatch
                      |>fun _ -> (false, buttonName)
                                 |> MakeButtonVisible
                                 |> dispatch

             | _ when pos = 0 && plusMinus = -1 -> ()

             | _ ->
                partSequence
                |> Seq.item (pos + plusMinus)
                |> fun (data,_) ->
                      data
                      |> NewPart2Show
                      |> dispatch
                      |> fun _ -> (true, buttonName)
                                  |> MakeButtonVisible
                                  |> dispatch

    let result = 
        partSequence
        |> Seq.tryFind (fun (data,_) -> data.Title = model.Data.Title)
                        |> function
                            | res when res = None -> ()
                            | res ->  res.Value
                                      |> fun (_,pos) -> partPositionChoices pos

    result


let whichNavigationButton buttonName =
    match buttonName with
    | "PreviousButton" -> -1
    | "NextButton" -> 1
    | _ -> 0
