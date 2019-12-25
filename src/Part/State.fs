module Part.State

open Controls
open Types
open Elmish

type Msg =
    | NewPart2Show of Data.partData
    | MakeButtonVisible of bool * string

let init () : Model * Cmd<Msg> =
    {
        NextButton = defaultAppearanceAttributes
        PreviousButton = defaultAppearanceAttributes
        Go2Instruction = defaultAppearanceAttributes
        Data = Data.allData |> Seq.item 0 |> fun x -> x.Data |> Seq.item 0 
    }, []

let update msg model : Model * Cmd<Msg> =
    match msg with
    | NewPart2Show data -> { model with Data = data } , []
    | MakeButtonVisible (isVisible,buttonChoice) ->
        match buttonChoice with
        | "NextButton" ->
            match isVisible with
            | true -> { model with NextButton =
                                    { model.NextButton with Visible = "visible"}}, []
            | false -> { model with NextButton =
                                    { model.NextButton with Visible = "hidden"}}, []
        | "PreviousButton" ->
            match isVisible with
            | true -> { model with NextButton =
                                    { model.PreviousButton with Visible = "visible"}}, []
            | false -> { model with NextButton =
                                    { model.PreviousButton with Visible = "hidden"}}, []

        | _ -> model, []

