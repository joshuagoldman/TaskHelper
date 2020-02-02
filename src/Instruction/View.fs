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

let modificationButtons model dispatch name =
    Html.a[
        prop.className "button"
        prop.style[
            style.backgroundColor.white
            style.fontSize 18
            style.borderRadius 10
            style.margin 5
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

let modElements ( part : Data.partData ) =
    Html.div[
        prop.className "column is-half"
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
                     ( instruction : Data.InstructionData ) =
    Html.div[
        prop.className "column is-half"
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
                                prop.type' "info"
                                prop.placeholder (
                                    findPartPosition
                                        part.Title
                                        instruction
                                    |> string
                                ) 
                                                        
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let allPartsView ( part : Data.partData )
                 ( instruction : Data.InstructionData )
                 ( visibility) =
    visibility
    |>function 
        | res when res = style.visibility.visible ->
            Html.div[
                prop.className "columns"
                prop.children[
                    modElements part
                    positionElements part instruction
                ]
            ]

        | _ ->
            Html.div[
                prop.className "columns"
                prop.children[
                    Html.div[
                        prop.className "columns is-one-quarter"
                    ]
                    Html.a[
                        prop.className "button"
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
                    Html.div[
                        prop.className "column"
                    ]
                ]
            ]
    
let instructionTitleView title =
    Html.div[
        prop.className "columns"
        prop.children[
            quarterDiv
            Html.div[
                prop.className "column is-half"
                prop.style[
                    style.margin 5
                ]
                prop.children[
                    str title
                ]
            ]
            quarterDiv
        ]
    ]

let showAllInstructionParts model dispatch =
    match model.CurrInstruction with
    | Ok partRes ->
        match model.CurrInstruction with
        |Ok instRes ->
            partRes.Data
            |> Seq.toList
            |> List.map (fun part ->
                            allPartsView part instRes model.PartNameModificationInput.Visible)
            |> List.append(
                   [
                       instructionTitleView instRes.Title
                   ]   
               )
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
                      prop.className "column is-one-quarter"
                      prop.children[
                          modificationButtons model dispatch "Modify"
                      ]
                  ]
                  Html.div[
                      prop.className "column is-half"
                      prop.style[
                        style.margin 5
                      ]
                  ]
                  Html.div[
                      prop.className "column"
                      prop.children[
                          modificationButtons model dispatch "Delete"
                      ]
                  ]
              ]
          ]
          |> fun c -> List.append [c]  (showAllInstructionParts model dispatch )  
    )
  ]
