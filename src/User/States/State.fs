module User.State

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Browser
open Global
open Types

let urlUpdate (result : UserPage option) model =
    match result with
    | None ->
        console.error("Error parsing url")
        model, Navigation.modifyUrl (toHashUser model.CurrentPage)
    | Some page ->
        { model with CurrentPage = page }, []

let init() : Model * Cmd<Msg> =
    {
        CurrentPage = InstructionSearch
        InstructionSearch = InstructionSearch.State.init() |> fun (a,_) -> a
        Category = Category.State.init() |> fun (a,_) -> a
        UserData =
            seq
                [
                    Instruction.State.init() |> fun (a,b) -> a
                ]
        Instruction = Instruction.State.init() |> fun (a,b) -> a
    }, []

let update msg model : Model * Cmd<User.Types.Msg> =
    match msg with
    | InstructionMsg msg ->
        let (instruction, instructionCmd) = Instruction.State.update msg model.Instruction
        { model with Instruction = instruction}, Cmd.map InstructionMsg instructionCmd
    | InstructionSearchMsg msg ->
        let (instructionSearch, instructionSearchCmd) = InstructionSearch.State.update msg model.InstructionSearch
        { model with InstructionSearch = instructionSearch }, Cmd.map InstructionSearchMsg instructionSearchCmd
    | CategorySearchMsg msg ->
        let (category, categoryCmd) = Category.State.update msg model.Category
        { model with Category = category }, Cmd.map CategorySearchMsg categoryCmd 
