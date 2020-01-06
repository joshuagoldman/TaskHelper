module NewAdd.State

open Elmish
open Controls
open Types
open Part
open Data
open NewAdd.Types

let init () : Model * Cmd<Msg> =
    {
       NewAddDropDown = defaultAppearanceAttributes
       VideoUpload = defaultAppearanceAttributes
       NewAddButton = defaultAppearanceAttributes
       AddText = defaultAppearanceAttributes
    }, []


let update msg model : NewAdd.Types.Model * Cmd<Msg>  =
    match msg with
    | StoreNewPartMsg (ev, msg) -> model, Cmd.none
    | AddNewPartMsg (ev, msg) -> model, Cmd.none
