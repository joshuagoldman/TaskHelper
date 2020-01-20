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
open User.Logic
open Browser

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
            prop.onTextChange (fun ev -> TextHasChanged ev |>
                                         (User.Types.InstructionSearchMsg >> dispatch))
        ]

let sss ( model : User.Types.Model )  =
    match model.Instruction.CurrPart.Data with
    | Ok res -> res.Title
    | Error err -> err

let searchResult ( model : User.Types.Model ) dispatch result =
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

                            prop.onClick (fun _ -> go2PartOrInstruction dispatch result)
                            prop.href ( Global.toHashUser (choosePage result) )
                            prop.children
                                [
                                    str (WritePartOrInstruction result)
                                ]
                        ]
                ]
        ]

let SearchResultErrorComponent ( model : User.Types.Model ) message =
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
                        str ( if model.InstructionSearch.SearchBar.Text = ""
                              then ""
                              else message ) 
                    ]
            ]
    ]

let getSearchResults ( model : User.Types.Model ) dispatch =
    let status = loadData model.UserData
    match status with
    | Ok result ->
        result.Instructions
        |> Seq.map (fun instr -> Instruction (instr,[]))
        |> Seq.append (result.Instructions
                       |> Seq.collect (fun instr -> instr.Data
                                                    |> Seq.map (fun part -> Part(part, [], instr, []) )))
        |> Seq.filter (fun info -> searchInfo info (model.InstructionSearch.SearchBar.Text.ToLower()))
        |> function
           | res when res |> Seq.length <> 0 ->
                res
                |> Seq.map (fun result -> searchResult model dispatch result)
                |> Seq.toList
           | _ -> SearchResultErrorComponent model ("No search results found")

    | Error err ->
        SearchResultErrorComponent model err
     


let root ( model : User.Types.Model ) dispatch =
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


