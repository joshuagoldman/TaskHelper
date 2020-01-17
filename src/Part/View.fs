module Part.View

open Fable.React
open Fable.React.Props
open Feliz
open State
open Logic
open Browser

let instrVideo (model : Part.Types.Model) dispatch =
    let instructionVideo =
        match model.Data with
        | Ok result ->result.InstructionVideo
        | Error err -> err

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
                    Src instructionVideo
                    Type "video/mp4"
                ]
            str "Your browser does not support the video"
        ]

let textArea (model : Part.Types.Model) dispatch =
    let instructionText =
        match model.Data with
        | Ok result ->result.InstructionTxt
        | Error err -> err

    Html.div
        [
            prop.style
                [
                    style.color.white
                    style.margin(20,0,100,0)
                ]

            prop.children
                [
                    str instructionText
                ]
        ]

let goToInstructionButton ( model : Part.Types.Model ) dispatch =
    Html.div
        [
            prop.className "column"
            prop.style
                [
                    style.margin(200,0,0,0)
                ]
            prop.children
                [
                    Html.a
                        [
                            prop.className "button"
                            prop.href (Global.toHashUser Global.Instruction )
                            prop.children
                                [
                                    str "Go To Instruction"
                                ]
                        ]  
                ]
        ]

let navigationButton ( model : Part.Types.Model )
                       dispatch
                       buttonName =


    Html.div
        [
            prop.className "column"
            prop.style
                [
                    style.margin(200,0,0,0)
                ]
            prop.children
                [
                    Html.a
                        [
                            prop.className "button"
                            prop.onClick (fun _ -> Logic.checkInstructionAvailability
                                                         model
                                                         dispatch
                                                         buttonName)
                            prop.style
                                [
                                    whichNavigationButton model buttonName
                                    |> fun x -> x.Visible
                                ]
                            prop.children
                                [
                                    str (buttonName.Replace("Button", ""))
                                ]
                        ]
                ]

        ]

let navigationButtons model dispatch =
    Html.div
        [
            prop.className "columns"
            prop.children
                [
                    navigationButton model dispatch "PreviousButton"
                    goToInstructionButton model dispatch
                    navigationButton model dispatch "NextButton"
                    
                ]
        ]

let root model dispatch =

    Html.div
        [
            prop.style
                [
                    style.backgroundColor.deepSkyBlue
                    style.width 800 
                ]
            prop.children
                [
                    instrVideo model dispatch
                    textArea model dispatch
                    navigationButtons model dispatch
                ]
        ]
