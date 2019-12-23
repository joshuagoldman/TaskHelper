module Part.View

open Fable.React
open Fable.React.Props
open Feliz

let instrVideo model dispatch =
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
                    Src "Videos/Video1.mp4"
                    Type "video/mp4"
                ]
            str "Your browser does not support the video"
        ]

let textArea model dispatch =
    Html.div
        [
            prop.style
                [
                    style.color.white
                    style.margin(20,0,100,0)
                ]

            prop.children
                [
                    str "This is some info that'll be available for the user, so that the user'll know what to do"
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
