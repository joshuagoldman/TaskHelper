module NewAdd.View

open Fable.React
open Fable.React.Props
open Browser
open Feliz
open Fable.ReactServer
open Fable.Core.JsInterop
open Fable.Import
open Fable.Core

let fileHandle (ev : Types.Event) model dispatch =
    let files = (ev.target?files : Types.FileList)
    console.log(files)

    let reader = FileReader.Create()
    reader.onload <- (fun e ->
        console.log(e.target?result))
    reader.readAsDataURL(files.[0])

    let mutable formData = FormData.Create()

    seq[0..files.length - 1]
    |> Seq.map (fun pos -> files.[pos])
    |> Seq.iter (fun file -> formData.append ("resume", file))

    console.log(formData.values)


    

let alternative model dispatch =
    
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
                        prop.onChange (fun ev -> fileHandle (ev : Types.Event) model dispatch)
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
                                    str "Choose files..."
                                ]
                            ]
                            Html.span[
                                prop.className "file-name"
                                prop.children[
                                    str "No file uplaoded"
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
        prop.children
            [
                alternative model dispatch
            ]
    ]

