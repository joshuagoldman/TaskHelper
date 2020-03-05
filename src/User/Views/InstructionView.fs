module Instruction.View

open Fable.Core
open Fable.React
open Fable.React.Props
open Types
open Feliz

let quarterDiv =
    Html.div[
        prop.className "column is-one-quarter"
        prop.style[
            style.margin 5
        ]
    ]

let addParts model dispatch =
    Html.div[
        prop.className "columns is-centered"
        prop.style[
            style.margin 5
        ]
        prop.children[
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
                                            str "Add files..."
                                        ]
                                    ]
                                    Html.span[
                                        prop.className "file-name"
                                        prop.children[
                                            str "No files uploaded"
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]
    |> fun x -> [
            x
            Global.divWithStyle
                (Some "columns is-centered")
                model.FileAddMsg.Text
                (prop.style[style.margin 5; style.color.black;style.fontWeight.bold])
        ]


let buttonActions name =
    function
    | _ when name = "Modify" ->
        ""
    | _ when name = "Save" ->
        ""
    | _ when name = "Delete" ->
        ""
    | _ ->
        ""

let modificationButtons ( model : Instruction.Types.Model )
                          dispatch
                          name
                          disable =
    Html.a[
        prop.className "button"
        prop.disabled disable
        prop.style[
            style.backgroundColor.white
            style.fontSize 18
            style.borderRadius 10
        ]
        prop.children[
            str name
        ]
    ]

let findPartPosition partTitle ( instruction : Data.InstructionData ) =
    Seq.zip instruction.Data [0..instruction.Data |> Seq.length |> fun x -> x - 1]
    |> Seq.tryFind (fun (part,_) -> part.Title = partTitle)
    |> function
        | res when res <> None ->
            res.Value
            |> fun (_,pos) -> pos
        | _ -> 0

let modElements ( part : Data.partData ) dispatch =
    Html.div[
        prop.className "column"
        prop.style[
            style.margin 5
        ]
        prop.children[
            Html.div[
                prop.className "field"
                prop.children[
                    Html.div[
                        prop.className "control"
                        prop.children[
                            Html.input[
                                prop.className "input is-info"
                                prop.onTextChange (fun str -> Logic.upDateName part dispatch str )
                                prop.type' "info"
                                prop.placeholder part.Title
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let positionElements ( part : Data.partData )
                     ( instruction : Data.InstructionData )
                       dispatch =
    Html.div[
        prop.className "column"
        prop.style[
            style.margin 5
        ]
        prop.children[
            Html.div[
                prop.className "field"
                prop.children[
                    Html.div[
                        prop.className "control"
                        prop.children[
                            Html.input[
                                prop.className "input is-info"
                                prop.onTextChange (fun str -> Logic.upDatePosition part dispatch str )
                                prop.type' "info"
                                prop.placeholder (
                                    findPartPosition
                                        part.Title
                                        instruction
                                    |> string
                                    |> fun x -> "Part position: " + x
                                ) 
                                                        
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let modCheckBox part dispatch =
    [
        Html.div[
            prop.className "columns is-vcentered"
            prop.style[
                style.margin 10
            ]
            prop.children[
                Html.label[
                    prop.className "checkbox"
                    prop.onCheckedChange (fun isChecked -> Logic.upDateChecked part dispatch isChecked)
                    prop.children[
                        Html.input[
                            prop.type'.checkbox
                        ]
                    ]
                ]
            ]
        ]
    ]


let allPartsView ( part : Data.partData )
                 ( instruction : Data.InstructionData )
                   visibility
                   dispatch=
    visibility
    |>function 
        | res when res = style.visibility.visible ->
            Html.div[
                prop.className "columns is-centered"
                prop.children(
                    modCheckBox part dispatch
                    |> List.append [positionElements part instruction dispatch]
                    |> List.append [modElements part dispatch]
                )
            ]

        | _ ->
            Html.div[
                prop.className "columns is-centered"
                prop.style[
                    style.margin 10
                ]
                prop.children[
                    Html.a[
                        prop.className "button"
                        prop.onClick (fun _ -> Logic.go2PartFromInstruction part instruction dispatch)
                        prop.href (Global.toHashUser Global.Part)
                        prop.style[
                            style.backgroundColor.white
                            style.fontSize 18
                            style.opacity 0.9
                            style.borderRadius 10
                        ]
                        prop.children[
                            str part.Title
                        ]
                    ]
                ]
            ]
    
let instructionTitleView title =
    Html.div[
        prop.className "columns is-centered"
        prop.children[
            quarterDiv
            Html.div[
                prop.className "column"
                prop.style[
                    style.margin 5
                    style.fontSize 23
                    style.color.black
                ]
                prop.children[
                    str title
                ]
            ]
            quarterDiv
        ]
    ]

let ShowFileUpload model dispatch =
    model.PartNameModificationInput.Visible
    |> function
        | res when res = style.visibility.hidden ->
            [Html.none]
        | _ ->  addParts model dispatch

let showAllInstructionParts model dispatch =
    match model.CurrInstruction with
    | Ok partRes ->
        match model.CurrInstruction with
        |Ok instRes ->
            partRes.Data
            |> Seq.toList
            |> List.map (fun part ->
                            allPartsView part instRes model.PartNameModificationInput.Visible dispatch)
            |> List.append [ instructionTitleView instRes.Title ]   
        | Error err ->
            [
                Html.div[
                    prop.children[
                        str "No part nor instruction given" 
                    ]
                ]  
            ]
    | Error err ->
        [
          Html.div[
              prop.children[
                  str "No part Given" 
              ]
          ]  
        ]

           
let root model dispatch =
  Html.div[
    prop.children(
          Html.div[
              prop.className "columns"
              prop.children[
                  Html.div[
                      prop.className "column"
                      prop.onClick (fun _ -> Logic.modifyOrNot model dispatch )
                      prop.children[
                          modificationButtons model dispatch "Modify" false
                      ]
                  ]
                  Html.div[
                      prop.className "column"
                      prop.onClick (fun _ -> Logic.startSaving model dispatch)
                      prop.children[
                          modificationButtons model dispatch "Save" model.DeleteButton.Disable
                      ]
                  ]
                  Html.div[
                      prop.className "column"
                      prop.children[
                          modificationButtons model dispatch "Delete" model.DeleteButton.Disable
                      ]
                  ]
              ]
          ]
          |> fun l -> List.append [l]  (showAllInstructionParts model dispatch )
    )
  ]
