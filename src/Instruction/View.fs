module Instruction.View

open Fable.Core
open Fable.React
open Fable.React.Props
open Types
open Feliz

let quarterDiv =
    Html.div[
        prop.className "column is-one-quarter"
    ]

let modificationButtons model dispatch name =
    Html.a[
        prop.className "button"
        prop.style[
            style.backgroundColor.white
            style.fontSize 18
            style.borderRadius 10
        ]
        prop.children[
            str name
        ]
    ]

let allPartsView ( part : Data.partData ) =
    Html.div
        [
            prop.className "row"
            prop.children[
                Html.a[
                    prop.className "button"
                    prop.style[
                        style.backgroundColor.white
                        style.margin(5,5,5,50)
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
        prop.className "row"
        prop.style[
            style.margin(5,5,5,50)
            style.fontSize 25

        ]
        prop.children[
            quarterDiv
            Html.div[
                prop.className "column is-half"
                prop.children[
                    str title
                ]
            ]
            quarterDiv
        ]
    ]

let showAllInstructionParts model dispatch =
    match model.CurrInstruction with
    | Ok res ->
        res.Data
        |> Seq.map (fun part ->
                        allPartsView part)
        |> Seq.append(
               seq[
                   instructionTitleView res.Title
               ]   
           )
    | Error err ->
        seq[
          Html.div[
              prop.children[
                  str err 
              ]
          ]  
        ]

let root model dispatch =
  Html.div[
    prop.className "rows"
    prop.children[
        Html.div[
            prop.className "row"
            prop.children[
                Html.div[
                    prop.className "columns"
                    prop.children[
                        Html.div[
                            prop.className "column is-one-quarter"
                            prop.children[
                                modificationButtons model dispatch "Delete"
                            ]
                        ]
                        Html.div[
                            prop.className "column is-half"                
                        ]
                        Html.div[
                            prop.className "column is-one-quarter"
                            prop.children[
                                modificationButtons model dispatch "Modify Instruction"
                            ]    
                        ]
                    ]
                ]
            ]
        ]
        Html.div[
            prop.className "row"
            prop.children(
                showAllInstructionParts model dispatch
            )   
        ]
    ]
  ]
