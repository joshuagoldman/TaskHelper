module Main.State

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Browser
open Global
open Types

let urlUpdate (result : MainPage option) model =
    match result with
    | None ->
        console.error("Error parsing url")
        model, Navigation.modifyUrl (toHashMain model.CurrentPage)
    | Some page ->
        { model with CurrentPage = page }, []

let init() : Model * Cmd<Msg> =
    {
        CurrentPage = InstructionSearch
        InstructionSearch = InstructionSearch.State.init() |> fun (a,_) -> a
    }, []

let update msg model : Model * Cmd<Main.Types.Msg> =
    match msg with
    | PartMsg msg ->
        let (instructionSearch, instructionSearchCmd) = InstructionSearch.State.update msg model.InstructionSearch
        {model with InstructionSearch = instructionSearch }, Cmd.map PartMsg instructionSearchCmd
    | InstructionSearchMsg msg ->
        let (instructionSearch, instructionSearchCmd) = InstructionSearch.State.update msg model.InstructionSearch
        {model with InstructionSearch = instructionSearch}, Cmd.map InstructionSearchMsg instructionSearchCmd
    | InstructionMsg msg ->
        let (instructionSearch, instructionSearchCmd) = InstructionSearch.State.update msg model.InstructionSearch
        {model with InstructionSearch = instructionSearch}, Cmd.map InstructionMsg instructionSearchCmd
