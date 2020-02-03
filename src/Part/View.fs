module Part.View

open Fable.React
open Fable.React.Props
open Feliz
open State
open Logic
open Browser

type MediaChoice =
    | Video
    | Txt

let instructionVideo (model : Part.Types.Model) choice =
    match model.UserId with
    | Ok idResult ->
        match model.Data with
        | Ok dataResult ->
            match choice with
            | Video ->
                dataResult.InstructionVideo
            | Txt ->
                dataResult.InstructionTxt
        | Error dataError -> dataError
    | Error idError -> idError

let userId (model : Part.Types.Model) =
    match model.UserId with
    | Ok result ->result |> string |> fun x -> x + "_"
    | Error err -> err

let instrVideo (model : Part.Types.Model) dispatch =

    video
        [
            Style
                [
                    Margin "5,50,20,20"

                ]
            Controls true
        ]
        [
            source
                [
                    Src  (instructionVideo model Video)
                    Type "video/mp4"
                ]
            str "Your browser does not support the video"
        ]

let instructionText (model : Part.Types.Model) =
    match model.Data with
    | Ok result ->result.InstructionTxt
    | Error err -> err

let goToInstructionButton ( model : Part.Types.Model ) dispatch =
    Html.div[
        prop.className "columns is-vcentered"
        prop.style[
            style.margin 5
        ]
        prop.children[
            Html.a[
                prop.className "button"
                prop.href (Global.toHashUser Global.Instruction )
                prop.children[
                        str "Go To Instruction"
                ]
            ] 
        ]    
    ]

let navigationButton ( model : Part.Types.Model )
                       dispatch
                       buttonName
                       position =
    Html.div[
        prop.className ("columns is-" + position)
        prop.style[
            style.margin 5
        ]
        prop.children[
            Html.a[
                prop.className "button"
                prop.onClick (fun _ -> Logic.checkInstructionAvailability
                                                model
                                                dispatch
                                                buttonName)
                prop.style[
                    whichNavigationButton model buttonName
                    |> fun x -> x.Visible
                ]
                prop.children[
                    str (buttonName.Replace("Button", ""))
                ]
            ]
        ]
    ]

let getInstruction ( model : Types.Model ) =
    match model.Instruction with
    | Ok res -> res.Title
    | Error err -> ""

let root model dispatch =
    Html.div[
        Html.div[
            prop.className "columns"
            prop.children[
                Html.div[
                    prop.className "column"
                ]
                Html.div[
                    prop.className "column is-half"
                    prop.children[
                        str (getInstruction model)
                    ]
                ]
                Html.div[
                    prop.className "column"
                ]
            ]
        ]
        Html.div[
            prop.className "columns"
            prop.children[
                Html.div[
                    prop.className "column is-full"
                    prop.style[
                        style.margin 5
                    ]
                    prop.children[
                        instrVideo model dispatch
                    ]
                ]
            ]
        ]
        Html.div[
            prop.className "columns"
            prop.children[
                Html.div[
                    prop.className "column is-full"
                    prop.style[
                        style.margin 5
                        style.color.white
                    ]
                    prop.children[
                        str (instructionText model)
                    ]
                ]
            ]
        ]
        Html.div[
            prop.className "columns"
            prop.style[
                style.margin 5
            ]
            prop.children[
                Html.div[
                    prop.className "column"
                    prop.children[
                        navigationButton model dispatch "PreviousButton" "vcentered"
                    ]
                ]
                Html.div[
                    prop.className "column"
                    prop.children[
                        goToInstructionButton model dispatch
                    ]
                ]
                Html.div[
                    prop.className "column"
                    prop.children[
                        navigationButton model dispatch "NextButton" "centered"
                    ]
                ]
            ]
        ]   
    ]
    
