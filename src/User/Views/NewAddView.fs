module NewAdd.View

open Fable.React
open Fable.React.Props
open Browser
open Feliz
open Fable.ReactServer
open Fable.Core.JsInterop
open Fable.Import
open Fable.Core

let alternative ( model : NewAdd.Types.Model ) dispatch name =
    
    Html.div[
        prop.className "file has-name"
        prop.children[
            Html.label[
                prop.className "file-label"
                prop.children[
                    Html.input[
                        prop.className "file-input"
                        prop.type'.file
                        prop.name "resume"
                        prop.multiple true
                        prop.onChange (fun ev -> User.Logic.fileHandle (ev : Types.Event)
                                                                        model.CurrentInstruction
                                                                        dispatch
                                                                        name)
                    ]
                    Html.span[
                        prop.className "file-cta"
                        prop.children[
                            Html.span[
                                prop.className "file-icon"
                                prop.children[
                                    Html.i[
                                        prop.className "fas fa-upload"
                                    ]
                                ]
                            ]
                            Html.span[
                                prop.className "file-label"
                                prop.children[
                                    str ("Choose " + name + "...")
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let chooseFilesNames ( model : NewAdd.Types.Model ) name =
    name
    |> function
        | res when res = "videos" ->
            model.VideosUploadInput.Text
        | _ ->
            model.InstructionTxtUploadInput.Text

let infoText ( model : NewAdd.Types.Model ) dispatch name =
    Html.div[
        prop.style[
            style.color.black
            style.fontStyle.italic
            style.fontWeight.bold
            style.textAlign.center
        ]
        prop.children(
            User.Logic.currFilesInfo model.NewInstructionData name
        )
    ]

let uploadDuet model dispatch name =
    seq[
        Html.div[
            prop.className "columns is-centered"
            prop.style[
                style.margin.auto
            ]
            prop.children[
                alternative model dispatch name
            ]
        ]
        Html.div[
            prop.className "columns is-centered"
            prop.style[
                style.margin 50
                style.opacity 0.5
            ]
            prop.children[
                infoText model dispatch name
            ]
        ]
    ]

let info ( model : NewAdd.Types.Model ) =
    Html.div[
        prop.style[
            style.marginLeft 30
            style.color.black
        ]
        prop.children(
            model.NewAddMessages
        )
    ]

let spinner ( model : NewAdd.Types.Model ) =
    Html.div[
        prop.className "columns is-centered"
        prop.style[
            style.marginTop 20
        ]
        prop.children[
            Html.i[
                prop.className "fa fa-cog fa-spin fa-2x"
            ]
        ]
    ]

let newInstructionSelected ( ev : Types.Event ) dispatch =
    let instrName = ev.target?value |> string
    instrName
    |> ( User.Types.NewAddNewCurrentInstruction >> dispatch)

let uploadButton model dispatch =
    Html.div[
        prop.className "columns is-vcentered"
        prop.style[
            style.margin 15
            style.color.black
        ]
        prop.children[
            Html.div[
                prop.className "control"
                prop.children[
                    Html.button[
                        prop.className "button is-light"
                        prop.onClick (fun _ -> User.Logic.isUploadable model dispatch)
                        prop.children[
                            str "Upload files"
                        ]
                    ]
                ]
            ]
            info model
            Html.div[
                prop.className "select"
                prop.onChange (fun ev -> newInstructionSelected (ev : Types.Event) dispatch)
                prop.children[
                    Html.select[
                        prop.children(
                            model.InstructionList
                            |> function
                                | res when res.IsSome ->
                                    res.Value
                                    |> Seq.map (fun instruction ->
                                        Html.option[
                                            str instruction
                                        ])
                                | _ -> seq[Html.none]
                            |> Seq.append(
                                    Html.option[
                                        str "New"
                                    ]
                                    |> fun x -> seq[x]
                                ) 
                        )
                    ]
                ]
            ]
        ]
    ]

let root model ( dispatch : User.Types.Msg -> unit ) =
    Html.div[
        prop.children(
            uploadDuet model dispatch "instructions"
            |> Seq.append (uploadDuet model dispatch "videos")
            |> Seq.append [ uploadButton model dispatch ]
        )
            
    ]
