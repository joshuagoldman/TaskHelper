module User.View

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Browser.Types
open Types
open Fable.React
open Global

open Fable.React
open Fable.React.Props
open Feliz

let menuItem label page currentPage =
    li
      [ ]
      [ a
          [ classList [ "is-active", page = currentPage ]
            Href (toHash page) ]
          [ str label ] ]

let chooseSideMenuHref name =
    match name with
    | "Search Instruction" -> Global.InstructionSearch
    | "Category" -> Global.Part
    | _ -> Global.InstructionSearch

let clearIfSearchButton dispatch name =
    match name with
    | "Search Instruction" ->  InstructionSearch.Types.ClearSearchResult
                               |> (InstructionSearchMsg >> dispatch)
    | _ -> ()

let menuButton model dispatch name =
    Html.div
        [
            prop.className "row"
            prop.children
                [
                    Html.a
                        [
                            prop.className "button"
                            prop.href (toHashUser (chooseSideMenuHref name))
                            prop.style
                                [
                                    style.backgroundColor ""
                                    style.margin 5
                                    style.fontSize 25
                                    style.border("2px", borderStyle.groove , "black")
                                ]
                            prop.onClick (fun _ -> clearIfSearchButton dispatch name)
                            prop.children
                                [
                                    str name
                                ]
                        ]
                ]
        ]


let menuLabel model dispatch =
    Html.p
        [
            prop.className "menu-label"
            prop.style
                [
                    style.margin 5
                    style.color.white
                    style.fontSize 25
                ]
            prop.children
                [
                    str "General"
                ]
        ]

let sideMenu model dispatch =
    Html.div
        [
            prop.className "rows"
            prop.children
                [   
                    menuButton model dispatch "Search Instruction"
                    menuButton model dispatch "Category"
                ]
        ]

let navbar model dispatch =
    Html.div
        [
            prop.className "row"
            prop.style
                [
                    style.margin 5
                    style.justifyContent.center
                    style.display.flex
                    style.fontSize 30.1
                    style.color.white
                ]
            prop.children
                [
                    str "Task Helper"
                ]
        ]

let bodyCols model dispatch =
    let pageHtml model =
        match model.CurrentPage with
            | Instruction ->

                Instruction.View.root model.Instruction ( InstructionMsg >> dispatch )

            | InstructionSearch ->

                InstructionSearch.View.root model dispatch

            | Part ->
                    Part.View.root model.Instruction.CurrPart ( Instruction.State.PartMsg >>
                                                                InstructionMsg >>
                                                                dispatch )
    
    Html.div
        [
            prop.className "columns"

            prop.children
                [
                    Html.div
                        [
                            prop.className "column"
                            prop.style
                                [
                                    style.margin(20,20,100,5)
                                ]
                            prop.children
                                [
                                    menuLabel model dispatch
                                    sideMenu model dispatch
                                ]
                        ]
                    Html.div
                        [
                            prop.className "column"
                            prop.style
                                [
                                    style.margin(30,400,100,5)
                                    style.backgroundColor.deepSkyBlue
                                    style.borderRadius 20
                                    style.opacity 0.9
                                ]
                            prop.children
                                [
                                    pageHtml model
                                ]
                        ] 
                ]
        ]

let body model dispatch =
    Html.div
        [
            prop.className "row"

            prop.children
                [
                    bodyCols model dispatch
                ]
        ]

let root model dispatch =

    Html.div
        [
            prop.className "rows"
            prop.style
                [
                    style.backgroundImage "url(img/Beach_Img.jpg)"
                    style.position.absolute
                    style.backgroundSize.cover


                ]
            prop.children
                [
                    navbar model dispatch
                    body model dispatch
                ] 
        ]


