module App.View

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Types
open Fable.React
open App.State
open Global

importAll "../sass/main.sass"

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

let menuButton model dispatch name =
    Html.div
        [
            prop.className "row"
            prop.children
                [
                    Html.a
                        [
                            prop.className "button"
                            prop.href (toHash (chooseSideMenuHref name)
                            prop.style
                                [
                                    style.backgroundColor ""
                                    style.margin 5
                                    style.fontSize 25
                                    style.border("2px", borderStyle.groove , "black")
                                ]
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
    let pageHtml =
        match model.CurrentPage with
            | Instruction -> Instruction.View.root model.Instruction (InstructionMsg >> dispatch)
            | InstructionSearch -> InstructionSearch.View.root model.InstructionSearch (InstructionSearchMsg >> dispatch)
            | Part -> Part.View.root model.Part (PartMsg >> dispatch)

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
                                    pageHtml
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
                    style.backgroundSize.cover
                    style.height 730
                    style.width 1380


                ]
            prop.children
                [
                    navbar model dispatch
                    body model dispatch
                ] 
        ]

open Elmish.React
open Elmish.Debug
open Elmish.HMR

// App
Program.mkProgram init update root
|> Program.toNavigable (parseHash pageParser) urlUpdate
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReactSynchronous "elmish-app"
|> Program.run
