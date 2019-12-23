module Part.View

open Fable.React
open Fable.React.Props
open Feliz

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
                    Src model.Video.Value
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
                    str model.InstructionTxt.Value
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
                ]
        ]
