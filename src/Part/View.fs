module Part.View

open Fable.React
open Fable.React.Props
open Feliz
open State
open Logic

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
                    Src model.Data.InstructionVideo
                    Type "video/mp4"
                ]
            str "Your browser does not support the video"
        ]

let textArea (model : Part.Types.Model) dispatch =
    Html.div
        [
            prop.style
                [
                    style.color.white
                    style.margin(20,0,100,0)
                ]

            prop.children
                [
                    str model.Data.InstructionTxt
                ]
        ]
    
let navigationButton ( model : Part.Types.Model ) dispatch buttonName =
    Html.a
        [
            prop.className "button"
            prop.onClick (fun _ -> buttonName
                                   |> whichNavigationButton
                                   |> go2PreviousOrNext model dispatch buttonName)
            prop.children
                [
                    str buttonName
                ]
        ]
let navigationButtons model dispatch =
    Html.div
        [
            prop.className "columns"
            prop.children
                [
                    navigationButton model dispatch "NextButton"
                    navigationButton model dispatch "PreviousButton"
                    
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
