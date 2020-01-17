module Instruction.View

open Fable.Core
open Fable.React
open Fable.React.Props
open Types
open Feliz

let simpleButton txt action dispatch =
  div
    [ ClassName "column is-narrow" ]
    [ a
        [ ClassName "button"
          OnClick (fun _ -> action |> dispatch) ]
        [ str txt ] ]

let root model dispatch =
  Html.div
    [
        prop.children
            [
                str model.InstructionErrorMessage.Text
            ]
    ]
