module InstructionSearch.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Feliz
open Types
open State
open System

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

let searchInfo (info : SearchResult) (keyWord : string) =
    match info with
    | Instruction (model, modelCmd) -> model.Title.ToLower().Contains keyWord && keyWord <> ""
    | Part (model, modelCmd) -> model.Title.ToLower().Contains keyWord && keyWord <> ""

let go2PartOrInstruction dispatch result =
    match result with
    | Part (partModel, _) ->
        Part.State.NewPart2Show partModel
        |> PartHasBeenClicked
        |> dispatch
    | Instruction (instructionModel, _) ->
        Instruction.Types.NewInstruction2Show instructionModel
        |> InstructionHasBeenClicked
        |> dispatch

let WritePartOrInstruction result =
    match result with
    | Part (model, modelCmd) -> model.Title
    | Instruction (model, modelCmd) -> model.Title

let searchResult model dispatch result =
    Html.a
        [
            prop.className "Button"
            prop.style
                [
                    style.backgroundColor.white
                    style.margin(5,5,5,50)
                    style.fontSize 25
                    style.opacity 0.9
                    style.borderRadius 10
                ]

            prop.onClick (fun _ -> go2PartOrInstruction dispatch result)
            prop.children
                [
                    str (WritePartOrInstruction result)
                ]
        ]


let getSearchResults model dispatch =
    allPartsAndInstructions
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
                                str "No results found"
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


