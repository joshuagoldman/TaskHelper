module Instruction.View

open Fable.Core
open Fable.React
open Fable.React.Props
open Types
open Feliz
open Browser
open Fable.Core
open Fable.Core.JsInterop

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

let modificationButtons ( model : Instruction.Types.Model<User.Types.Msg> )
                          dispatch
                          name =
    Html.a[
        prop.className "button"
        prop.onClick (fun ev ->
                match name with
                | "Modify" ->
                        Logic.modifyOrNot model dispatch
                | _ ->
                    ()
                    |>function
                        | _ when model.PartNameModificationInput.Disable = false ->
                            match model.UserTypeDispatch with
                            | Data.UsrTypeDispatchOptions.DispatchDefined usrTypeDispatch ->
                                let utils =
                                    {
                                        Data.Positions = User.Logic.getPositions ev
                                        Data.MsgDispatch = usrTypeDispatch
                                    }
                        
                                match name with
                                | "Save" ->
                                    utils
                                    |>  ( Instruction.Types.SaveInstructionToDataBase >> dispatch)
                                | "Modify Names" ->
                                    Logic.modifyNames model dispatch utils
                                | "Reset" ->
                                    match model.CurrInstruction with
                                    | Ok (instruction,_) ->
                                        match instruction.Title with
                                        | Data.InstructionTitleInfo.HasOldName title ->
                                            title
                                            |> ( ResetInstruction
                                            >> dispatch )
                                        | Data.InstructionTitleInfo.HasNewName names ->
                                            names.DbName
                                            |> ( ResetInstruction
                                            >> dispatch )
                                    | _ -> ()
                                | _ ->
                                    utils
                                    |>  ( Instruction.Types.CreateDeletePopup >> dispatch)
                        
                            | _ -> ()
                        | _ -> ())
        prop.disabled(
            match name with
            | "Modify" -> false
            | _ -> model.PartNameModificationInput.Disable)
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


    

let modElements ( part : Data.partData ) model dispatch =
    let modificationControl =
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
                                    prop.onTextChange (fun str -> Logic.partNameToChange dispatch part str )
                                    prop.onMouseEnter(fun ev ->
                                        match model.UserTypeDispatch with
                                        | Data.UsrTypeDispatchOptions.DispatchDefined usrTypeDispatch ->
                                            let utils =
                                                {
                                                    Data.Positions = User.Logic.getPositions ev
                                                    Data.MsgDispatch = usrTypeDispatch
                                                }
                                            (part,utils,style.visibility.visible) 
                                            |> (Instruction.Types.HoverPartMsg >> dispatch)
                                        | _ -> ())
                                    prop.onMouseLeave(fun ev ->
                                        match model.UserTypeDispatch with
                                        | Data.UsrTypeDispatchOptions.DispatchDefined usrTypeDispatch ->
                                            let utils =
                                                {
                                                    Data.Positions = User.Logic.getPositions ev
                                                    Data.MsgDispatch = usrTypeDispatch
                                                }
                                            (part,utils,style.visibility.hidden) 
                                            |> (Instruction.Types.HoverPartMsg >> dispatch)
                                        | _ -> ())
                                    prop.type'.text
                                    prop.value ( Logic.newnameValue model part )
                                    prop.placeholder part.Title
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]

    Instruction.Logic.enableModificationTestable model.CurrPositions part
    |> function
        | res when res = true ->
            modificationControl
        | _ -> Html.none

let partPositionoptions model
                        ( currPart : Data.partData ) =
    match model.CurrPositions with
    | Some modInfo ->
        modInfo
        |> Seq.map (fun info ->
            let delOrReg =
                match info.DelOrReg with
                | Some button ->
                    match button with
                    | Delete _ -> true
                    | Regret _ -> false
                | _ -> false
            ()
            |> function
                | _ when info.Names.CurrName.Trim() <> currPart.Title.Trim() &&
                         delOrReg = true ->
                    Some ( info.Names.CurrName.Trim() )
                | _ -> None
        )
        |> Seq.choose id
        |> Seq.map (fun partTitle ->
            Html.option[ 
                prop.children[
                    str partTitle
                ]
            ])
        |> Seq.append(
            seq[
                Html.option[                    
                    prop.children[
                        str "Select new part"
                    ]
                ]
            ]
        ) 
    | _ ->
        Html.option[
            prop.children[
                str "Select new part"
            ]
        ]
        |> fun x -> seq[x]

let positionElements ( part : Data.partData )
                     ( instruction : Data.InstructionData )
                       model
                       dispatch =

    let modificationControls =
        Html.div[
            prop.className "column"
            prop.style[
                style.margin 5
            ]
            prop.children[
                Html.div[
                    prop.className "field"
                    prop.children[
                        Html.label[
                            prop.className "control"
                            prop.children[
                                Html.div[
                                    prop.className "select"
                                    prop.onChange (fun ev -> Logic.newPartSelected (ev : Types.Event) part.Title dispatch)
                                    prop.children[
                                        Html.select[
                                            prop.children(
                                                partPositionoptions model part
                                            )
                                        ]
                                    ]
                                ]
                            ]

                        ]
                    ]
                ]
            ]
        ]

    Instruction.Logic.enableModificationTestable model.CurrPositions part
    |> function
        | res when res = true ->
            modificationControls
        | _ -> Html.none

let delOrRegbutton model part dispatch =
    let button buttonName =
        [
            Html.div[
                prop.className "columns is-vcentered"
                prop.style[
                    style.margin 10
                ]
                prop.children[
                    Html.label[
                        prop.className "button is-link"
                        prop.style[
                        ]
                        prop.onClick (fun _ ->  Logic.dispatchDelOrReg model dispatch part)
                        prop.children[
                            str buttonName
                        ]
                    ]
                ]
            ]
        ]

    let validOrNot = Logic.getDelOrRegName model dispatch part

    match validOrNot with
    | Some buttonName ->
        button buttonName
    | _ -> [Html.none]


let instructionTitleView model dispatch titleAlt =
    model.PartNameModificationInput.Visible
    |> function
        | res when res = style.visibility.visible ->
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
                                        prop.onTextChange (fun str -> Logic.instructionNameToChange dispatch titleAlt str )
                                        prop.type'.text
                                        prop.value ( Logic.newInstructionName model titleAlt )
                                        prop.placeholder ( titleAlt |> Logic.getTitle )
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        | _ ->
            Html.div[
                prop.className "columns is-centered"
                prop.children[
                    Html.div[
                        prop.className "column is-5"
                        prop.style[
                            style.fontSize 23
                            style.fontFamily "Comic Sans MS"
                            style.fontWeight.bold
                        ]
                        prop.children[
                            str ( titleAlt |> Logic.getTitle )
                        ]
                    ]
                ]
            ]
    
let allPartsView ( part : Data.partData )
                 ( instruction : Data.InstructionData )
                 ( model : Instruction.Types.Model<User.Types.Msg> )
                   dispatch=
    model.PartNameModificationInput.Visible
    |>function 
        | res when res = style.visibility.visible ->
            Html.div[
                prop.className "columns is-centered"
                prop.children(
                    delOrRegbutton  model part dispatch 
                    |> List.append [positionElements part instruction model dispatch]
                    |> List.append [modElements part model dispatch]
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
        |Ok (instRes,_) ->
            (instRes.Data)
            |> Seq.toList
            |> List.map (fun part ->
                            allPartsView part instRes model dispatch)
            |> List.append [ instructionTitleView model dispatch instRes.Title ]
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

let modificationElements model dispatch =
    model.PartNameModificationInput.Visible
    |> function
        | visibility when visibility = style.visibility.visible ->
            Html.div[
                prop.className "columns is-centered"
                prop.style[
                    model.PartNameModificationInput.Visible
                ]
                prop.children[
                    Html.div[
                        prop.className "column is-1"
                        ]
                    Html.div[
                        prop.className "column"
                        prop.children[
                            modificationButtons model dispatch "Modify Names"
                        ]
                    ]
                    Html.div[
                        prop.className "column is-4"
                        prop.children[
                            modificationButtons model dispatch "Reset"
                        ]
                    ]
                ]
            ]
        | _ -> Html.none
  
let root model dispatch =
  Html.div[
    prop.children(
          Html.div[
              prop.className "columns"
              prop.children[
                  Html.div[
                      prop.className "column"
                      prop.children[
                          modificationButtons model dispatch "Modify"
                      ]
                  ]
                  Html.div[
                      prop.className "column is-3"
                      prop.children[
                          modificationButtons model dispatch "Save"
                      ]
                  ]
                  Html.div[
                      prop.className "column"
                      prop.disabled model.PartNameModificationInput.Disable
                      prop.children[
                          modificationButtons model dispatch "Delete Instruction"
                      ]
                  ]
              ]
          ]
          |> fun x ->
            seq[x
                modificationElements model dispatch
            ]
          |> fun l -> Seq.append l (showAllInstructionParts model dispatch )
    )
  ]
