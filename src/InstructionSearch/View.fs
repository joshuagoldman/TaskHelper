module InstructionSearch.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Feliz
open Types
open State
open System
open Data

let searchBarName model dispatch =
    Html.div
        [
            prop.className "field"
            prop.children
                [
                    Html.label
                        [
                            prop.className "label"
                            prop.style
                                [
                                    style.fontSize 20
                                    style.color.white
                                ]
                            prop.children
                                [
                                    str "Search"
                                ]
                        ]
                    Html.div
                        [
                            prop.className "control"
                        ]
                ]
        ]

let searchBar model dispatch =
    Html.input
        [
            prop.className "input"
            prop.style
                [
                    style.margin(0,0,30,0)
                ]
            prop.type' "text"
            prop.placeholder "Text input"
            prop.onTextChange (fun ev -> TextHasChanged ev |> dispatch)
        ]

let searchInfo info (keyWord : string) =
    match info with
    | Instruction (model, _) -> model.Title.ToLower().Contains keyWord && keyWord <> ""
    | Part (model, _) -> model.Title.ToLower().Contains keyWord && keyWord <> ""

let go2PartOrInstruction dispatch result =
    match result with
    | Part (partModel, _) ->
        Part.State.NewPart2Show partModel
        |> PartMsgIS
        |> dispatch
        |> fun _ -> Part.Logic.go2PreviousOrNext partModel (PartMsgIS >> dispatch) "" 
    | Instruction (instructionModel, _) ->
        Instruction.State.NewInstruction2Show instructionModel
        |> InstructionMsgIS
        |> dispatch

let WritePartOrInstruction result =
    match result with
    | Part (instruction, _) -> instruction.Title
    | Instruction (data, _) -> data.Title

let choosePage page =
    match page with
    | Part (_,_) -> Global.Part
    | Instruction (_,_) -> Global.Instruction

let searchResult model dispatch result =
    Html.div
        [
            prop.className "row"
            prop.children
                [
                    Html.a
                        [
                            prop.className "button"
                            prop.style
                                [
                                    style.backgroundColor.white
                                    style.margin(5,5,5,50)
                                    style.fontSize 25
                                    style.opacity 0.9
                                    style.borderRadius 10
                                ]
                            prop.href (Global.toHashMain (choosePage result) )
                            prop.onClick (fun _ -> go2PartOrInstruction dispatch result)
                            prop.children
                                [
                                    str (WritePartOrInstruction result)
                                ]
                        ]
                ]
        ]

let getSearchResults model dispatch =
    let instructionResults =
        allData
        |> Seq.map (fun instruction -> Instruction(instruction, []))
    let partResults =
        allData
        |> Seq.collect (fun instruction -> instruction.Data
                                           |> Seq.map (fun part -> Part(part, [])))
    Seq.append instructionResults partResults
    |> Seq.filter (fun info -> searchInfo info (model.SearchBar.Text.ToLower()))
    |> function
       | res when res |> Seq.length <> 0 ->
            res
            |> Seq.map (fun result -> searchResult model dispatch result)
            |> Seq.toList
       | _ ->
            [
                Html.div
                    [
                        prop.style
                            [
                                style.color.white
                                style.fontSize 25
                                style.margin(5,5,5,50)
                            ]
                        prop.children
                            [
                                str ( if model.SearchBar.Text = ""
                                      then ""
                                      else "No results found" ) 
                            ]
                    ]
            ]

let root model dispatch =
    let searchResults = getSearchResults model dispatch

    let searchBarList = 
        [
            searchBarName model dispatch
            searchBar model dispatch
        ]

    Html.div
        [
            prop.className "rows"
            prop.children
                (List.append searchBarList searchResults)  
        ]


