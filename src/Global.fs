module Global

open Feliz
open Fable.React
open Browser
open Fable.Core

type UserPage =
    | Part
    | Instruction
    | InstructionSearch
    | NewAdd

type Page =
    | Login
    | User of UserPage 

let toHashUser page =
    match page with
    | Part -> "#part"
    | Instruction -> "#instruction"
    | InstructionSearch -> "#search"
    | NewAdd -> "#newAdd"

let toHash page =
    match page with
    | Login -> "#login"
    | User userPage -> toHashUser userPage

/// <summary>A simple div with custom properties.</summary>
///<c>infotext,style</c>
///<remarks>logindelay</remarks>
let divWithStyle ( className : Option<string> ) msg properties =
    Html.div[
        prop.className (if className.IsSome then className.Value else "columns is-centered")
        prop.children[
            Html.div[
                prop.className "column"
            ]
            Html.div[
                prop.className "column is-11"
                properties
                prop.children[
                    str msg
                ]
            ]
        ]
    ]

let divWithFileName ( file : Types.File ) =
    Html.div[
        prop.className "columns is-centered"
        prop.children[
            Html.div[
                prop.className "column"
                prop.children[
                    divWithStyle
                        None
                        file.name
                        (prop.style[Feliz.style.color "black" ; Feliz.style.fontWeight 700])
                ]
            ]
       ]
    ]

