module Part.State

open Controls
open Types
open Elmish
open Feliz

let init () : Model * Cmd<Msg> =
    {
        NextButton = defaultAppearanceAttributes
        PreviousButton = defaultAppearanceAttributes
        Go2Instruction = defaultAppearanceAttributes
        Data = Error "No part is given"
        Instruction = Error "No Part is given"
        UserId = Error ""
    }, []

let update msg model : Model * Cmd<Msg> =
    match msg with
    | NewPart2Show (data, instruction) ->
            { model with Data = Ok data ;
                         NextButton =
                             { model.NextButton with Visible = style.visibility.visible } ;
                         PreviousButton =
                             { model.PreviousButton with Visible = style.visibility.visible }
                         Instruction = Ok instruction }, [] 

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

    | SendErrorMessage msg ->
        { model with Data = Ok (Data.errorPart) ; Instruction = Ok (Data.errorInstruction) }, []
                        

