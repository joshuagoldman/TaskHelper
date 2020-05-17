module User.View

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Browser.Types
open Fable.React
open Global
open NewAdd

open Fable.React
open Fable.React.Props
open Feliz
open Browser

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
    | "Current Instruction" -> Global.Instruction
    | "Current Part" -> Global.Part
    | "Upload/add to instruction" -> Global.NewAdd
    | _ -> Global.InstructionSearch

let clearIfSearchButton dispatch name =
    let clearNewAddInfoMsg =
        Html.none
        |> fun y -> [|y|]
        |> (NewAdd.Types.NewAddInfoMsg >>
            User.Types.NewAddMsg)

    match name with
    | "Search Instruction" ->  InstructionSearch.Types.ClearSearchResult
                               |> User.Types.InstructionSearchMsg
                               |> fun x ->
                                    seq[
                                        x
                                        clearNewAddInfoMsg
                                    ]
                                    |> Seq.iter (fun msg -> msg |> dispatch )
    | _ ->
         seq[
            clearNewAddInfoMsg
         ]
         |> Seq.iter (fun msg -> msg |> dispatch )

let menuButton model dispatch name =

    Html.a[
        prop.className "button"
        prop.href (toHashUser (chooseSideMenuHref name))
        prop.style[
            style.backgroundColor ""
            style.margin 5
            style.fontSize 25
            style.border("2px", borderStyle.groove , "black")
        ]
        prop.onClick (fun _ -> clearIfSearchButton dispatch name)
        prop.children[
                str name
        ]
    ]


let menuLabel model dispatch =
    Html.p[
        prop.className "menu-label"
        prop.style[
            style.margin 5
            style.color.white
            style.fontSize 25
        ]
        prop.children[
            str "General"
        ]
    ]

let sideMenu model dispatch =
    Html.div[
        menuButton model dispatch "Search Instruction"
        menuButton model dispatch "Current Instruction"
        menuButton model dispatch "Current Part"
        menuButton model dispatch "Upload/add to instruction"
    ]

let pageHtml ( model : User.Types.Model ) (dispatch : User.Types.Msg -> unit ) =
    match model.CurrentPage with
        | Instruction ->
            Instruction.View.root model.Instruction ( User.Types.InstructionMsg >> dispatch )

        | InstructionSearch ->
            InstructionSearch.View.root model dispatch

        | Part ->
                Part.View.root model.Instruction.CurrPart ( Instruction.Types.PartMsg >>
                                                            User.Types.InstructionMsg >>
                                                            dispatch )

        | NewAdd ->
            NewAdd.View.root model.NewAdd dispatch

let bodyCols model dispatch =   
    [
        Html.div[
            prop.className "column is-one-quarter"
            prop.style[
                    style.margin 30
            ]
            prop.children[
                    menuLabel model dispatch
                    sideMenu model dispatch
            ]
        ]
        Html.div[
            prop.className "column"
            prop.style[
                style.backgroundColor.deepSkyBlue
                style.borderRadius 20
                style.opacity 0.9
            ]
            prop.children[
                    pageHtml model dispatch
            ]
        ]
        Html.div[
            prop.className "column"
        ]
    ]

let body model dispatch =
    Html.div[
        prop.className "columns is-mobile"

        prop.children(
            bodyCols model dispatch
        )   
    ]

let navbar model dispatch =
    Html.div
        [
            prop.className "columns"
            prop.children[
                Html.div[
                    prop.className "column is-one-fifth"
                    prop.style
                        [
                            style.margin 20
                            style.fontSize 30.1
                            style.color.white
                            style.fontWeight.bold
                            style.textDecoration.underline
                            style.fontFamily "Comic Sans MS"
                        ]
                    prop.children
                        [
                            str "Task Helper"
                        ]
                ]
                Html.div[
                    prop.className "column"
                ]
            ]
            
        ]

let popup ( model : User.Types.Model) dispatch =
    match model.PopUp with
    | Some popup ->
        let buttons =
            match popup.ButtonSettings with
            | Some settings ->
                match popup.ClickMessages with
                | Some clickMessages ->
                    Html.div[
                        prop.className "columns is-centered"
                        prop.children[
                            Html.div[
                                prop.className "column"
                                prop.children[
                                    Html.a[
                                        prop.className "button"
                                        prop.style ( settings |> Seq.toList )
                                        prop.onClick (fun _ ->
                                            let removePopUpMsg =
                                                None
                                                |> User.Types.PopUpMsg
                                                |> fun x -> seq[x]
                                            clickMessages
                                            |> Seq.append removePopUpMsg
                                            |> Seq.iter (fun msg -> msg |> dispatch))
                                        prop.children[
                                            Fable.React.Helpers.str "Yes"
                                        ]
                                    ]
                                ]
                            ]
                            Html.div[
                                prop.className "column"
                                prop.children[
                                    Html.a[
                                        prop.className "button"
                                        prop.style ( settings |> Seq.toList )
                                        prop.onClick (fun _ -> None |> ( User.Types.PopUpMsg >> dispatch ) )
                                        prop.children[
                                            Fable.React.Helpers.str "No"
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
                            Html.a[
                                prop.className "button"
                                prop.style ( settings |> Seq.toList )
                                prop.onClick (fun _ -> None |> ( User.Types.PopUpMsg >> dispatch ) )
                                prop.children[
                                    Fable.React.Helpers.str "Ok"
                                ]
                            ]
                        ]
                    ]
            | _ -> Html.none
        Html.div[
            popup.Style 
            prop.children(
                (seq[buttons])
                |> Seq.append (popup.Messages |> Array.toSeq)
            )
        ]
    | None -> Html.none

let root model dispatch  =
    Html.div[
        prop.style[
            style.backgroundImage "url(img/Beach_Img.jpg)"
            style.position.absolute
            style.width 1500
            style.height 1024
        ]
        prop.children[
            popup model dispatch
            navbar model dispatch
            body model dispatch
        ]
    ]
        
                
 

