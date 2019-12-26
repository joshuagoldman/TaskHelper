module Part.State

open Controls
open Types
open Elmish
open Feliz

type Msg =
    | NewPart2Show of Data.partData
    | MakeButtonVisible of bool * string
    | ExampleMsg of string

let init () : Model * Cmd<Msg> =
    {
        NextButton = defaultAppearanceAttributes
        PreviousButton = defaultAppearanceAttributes
        Go2Instruction = defaultAppearanceAttributes
        Data = Data.allData |> Seq.item 0 |> fun x -> x.Data |> Seq.item 0 
    }, []

let update msg model : Model * Cmd<Msg> =
    match msg with
    | NewPart2Show data -> { model with Data = data ;
                                        NextButton =
                                            { model.NextButton with Visible = style.visibility.visible } ;
                                        PreviousButton =
                                            { model.PreviousButton with Visible = style.visibility.visible }} , []
    | MakeButtonVisible (isVisible,buttonChoice) ->
        match buttonChoice with
        | "NextButton" ->
            match isVisible with
            | true -> { model with NextButton =
                                    { model.NextButton with Visible = style.visibility.visible}}, []
            | false -> { model with NextButton =
                                    { model.NextButton with Visible = style.visibility.hidden}}, []
        | "PreviousButton" ->
            match isVisible with
            | true -> { model with PreviousButton =
                                    { model.PreviousButton with Visible = style.visibility.visible}}, []
            | false -> { model with PreviousButton =
                                    { model.PreviousButton with Visible = style.visibility.hidden}}, []

        | _ -> model, []

    | ExampleMsg str -> { model with Data =
                                        { model.Data with Title = str }}, []

