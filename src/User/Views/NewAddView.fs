module NewAdd.View

open Fable.Core
open Fable.React
open Fable.React.Props
open Feliz
open Browser.Event
open Browser.WebStorage
open Browser

let fileHanflingEventHandler ( filelist : Browser.Types.FileList)  =
    ignore |> fun _ -> ()

let fileUploadSpan model dispatch =
    [
        Html.span
            [
                prop.className "file-icon"
                prop.children
                    [
                        Html.i
                            [
                                prop.className "fas fa-upload"
                            ]
                    ]
            ]
        Html.span
            [
                prop.className "file-icon"
                prop.children
                    [
                        Html.i
                            [
                                prop.className "file-label"
                                prop.children
                                    [
                                        str "choose files"
                                    ]
                            ]
                    ]
            ]  
    ]

let fileUploadInputCta model dispatch =
    [
        Html.input
            [
                prop.className "file-input"
                prop.type'.file
                prop.name "resume"
            ]
        Html.span
            [
                prop.className "file-cta"
                prop.children
                    (fileUploadSpan model dispatch)
            ]
    ]


let fileUploadLabel model dispatch =
    Html.label
        [
            prop.className "file-label"
            prop.children
                ( fileUploadInputCta model dispatch )
        ]

let fileUpload model dispatch =
    Html.div
        [
            prop.className "file"
            prop.children
                [
                    fileUploadLabel model dispatch
                ]
        ]

let selectMode model dispatch =
    Html.div
        [
            prop.className "field"
            prop.children
                [
                    Html.div
                        [
                            prop.className "control"
                            prop.children
                                [
                                    Html.div
                                        [
                                            prop.className "select is-primary"
                                            prop.children
                                                [
                                                    Html.select
                                                        [
                                                            Html.option
                                                                [
                                                                    str "New"
                                                                ]
                                                            Html.option
                                                                [
                                                                    str "Add"
                                                                ]
                                                        ]
                                                ]
                                        ]
                                ]
                        ]
                ]
        ]

let root model dispatch =
  Html.div
    [
        prop.className ""
        prop.children
            [
                selectMode model dispatch
            ]
    ]

