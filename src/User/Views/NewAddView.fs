module NewAdd.View

open Fable.React
open Fable.React.Props
open Browser
open Feliz
open Fable.ReactServer
open Fable.Core.JsInterop
open Fable.Import
open Fable.Core

let fileHandle (ev : Types.Event) model dispatch name =
    let files = (ev.target?files : Types.FileList)
    console.log(files)


    seq[0..files.length - 1]
    |> Seq.map (fun pos -> User.Logic.chooseMediaByName name files.[pos])
    |> fun medias -> (medias,name)
    |> NewAdd.Types.NewFilesChosenMsg |> dispatch 

let alternative model dispatch name =
    
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
                        prop.onChange (fun ev -> fileHandle (ev : Types.Event) model dispatch name)
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

let uploadTriplet model dispatch name =
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

let progressBar ( model : NewAdd.Types.Model ) dispatch =
    Html.progress[
        prop.className "progress is-primary"
        prop.style[
            model.Progressbar.Visible
        ]
        prop.value 15
        prop.max 100
        prop.children[
            str "15"
        ]
    ]

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
            progressBar model dispatch
        ]
    ]

let root model dispatch =
    Html.div[
        prop.children(
            uploadTriplet model dispatch "instructions"
            |> Seq.append (uploadTriplet model dispatch "videos")
            |> Seq.append [ uploadButton model dispatch ]
        )
            
    ]
