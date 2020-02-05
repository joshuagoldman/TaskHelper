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
        prop.className "columns is-centered"
        prop.style[
            style.margin 10
            style.color.black
        ]
        prop.children(
            model.NewAddMessages
        )
    ]

let decideIfRightFormat ( medias : seq<NewAdd.Types.MediaChoiceFormData>) =
    medias
    |> Seq.filter (fun media ->
        match media with
        | NewAdd.Types.Video vid ->
            vid.``type`` <> "video/mpeg" &&
            vid.``type`` <> "video/ogg" &&
            vid.``type`` <> "video/mp4" &&
            vid.``type`` <> "video/avi"

        | NewAdd.Types.InstructionTxt instrctn ->
            instrctn.``type`` <> "text/markdown")
    |> function
        | res when ( res |> Seq.length ) = 0 ->
            None
        | res ->
            let initialMessage =
                seq[str "The following files did not have the the right file type:\n"]
            let secondMessage = 
                res
                |> Seq.map (fun media ->
                    match media with
                    | NewAdd.Types.Video vid ->
                        seq[ str (vid.name + "\n") ; Html.br[] ]

                    | NewAdd.Types.InstructionTxt instrctn ->
                        seq[ str(instrctn.name + "\n") ; Html.br[] ])
                |> Seq.collect (fun components -> components)
            let finalMessage =
                seq[str "Allowed file types are:\n"]

            initialMessage
            |> Seq.append secondMessage
            |> Seq.append finalMessage
            |> Some
                 

let decideIfUploadableByTypeCount ( medias : seq<NewAdd.Types.MediaChoiceFormData>) =
    let mutable videos = seq[]
    let mutable instructions = seq[]
    medias
    |> Seq.iter (fun media ->
        match media with
        | NewAdd.Types.Video vid ->
            videos <- videos |> Seq.append [vid]
        | NewAdd.Types.InstructionTxt instrctn ->
            instructions <- instructions |> Seq.append [instrctn])

    ()
    |> function
        | _ when ( videos |> Seq.length ) = ( instructions |> Seq.length ) ->
            None
        | _ ->
            seq[str "צריך לבחור אותו כמות של קבצי וידאו ומארקדבן"]
            |> Some 

let decideIfUploadValid ( medias : seq<NewAdd.Types.MediaChoiceFormData>)
                          model
                          dispatch =

    seq[decideIfUploadableByTypeCount medias]
    |> Seq.append [decideIfRightFormat medias]
    |> Seq.filter (fun msgs ->
        match msgs with
        | Some _ -> true
        | None -> false)
    |> function
        | res when ( res |> Seq.length = 0 ) ->
            seq[str ""] |> ( NewAdd.Types.NewAddInfoMsg >> dispatch )
        | res ->
            res
            |> Seq.collect (fun msgs -> msgs.Value)
            |> ( NewAdd.Types.NewAddInfoMsg >> dispatch )
  

let isUploadable ( model : NewAdd.Types.Model )
                   dispatch =
    match model.NewInstructionData with
    | Some res ->
        decideIfUploadValid res model dispatch
    | None ->
        seq[str "לא היה הבחרת קביצה"]
        |> ( NewAdd.Types.NewAddInfoMsg >> dispatch )

let uploadButton model dispatch =
    Html.div[
        prop.className "columns is-vcentered"
        prop.style[
            style.margin 20
            style.color.black
        ]
        prop.children[
            Html.div[
                prop.className "control"
                prop.children[
                    Html.button[
                        prop.className "button is-light"
                        prop.onClick (fun _ -> isUploadable model dispatch)
                        prop.children[
                            str "Upload files"
                        ]
                    ]
                ]
            ]
            info model
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
