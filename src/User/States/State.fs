module User.State

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Browser
open Global
open Data
open Types

let urlUpdate (result : UserPage option) model =
    match result with
    | None ->
        console.error("Error parsing url")
        model, Navigation.modifyUrl (toHashUser model.CurrentPage)
    | Some page ->
        { model with CurrentPage = page }, []

let initInstruction =
    seq
        [
            allData "" |> Seq.item 0
        ]

let loadInstructionItems = async {
        do! Async.Sleep 1000
        return LoadedInstructions (Finished (Ok initInstruction))
    }

let init() : Model * Cmd<Msg> =
    {
        Username = ""
        Password = ""
        CurrentPage = InstructionSearch
        InstructionSearch = InstructionSearch.State.init() |> fun (a,_) -> a
        Category = Category.State.init() |> fun (a,_) -> a
        UserData =
            Data.HasNostStartedYet 
        Instruction = Instruction.State.init() |> fun (a,b) -> a
    }, Cmd.ofMsg (LoadedInstructions Started)

let update msg model : Model * Cmd<User.Types.Msg> =
    match msg with
    | LoadedInstructions Started ->
        { model with UserData = InProgress }, Cmd.fromAsync loadInstructionItems
    | LoadedInstructions (Finished (Error error)) ->
        { model with UserData = Resolved ( Error error)}, Cmd.none
    | LoadedInstructions (Finished (Ok items)) ->
        { model with UserData = Resolved ( Ok items)}, Cmd.none
    | InstructionMsg msg ->
        let (instruction, instructionCmd) = Instruction.State.update msg model.Instruction
        { model with Instruction = instruction}, Cmd.map InstructionMsg instructionCmd
    | InstructionSearchMsg msg ->
        let (instructionSearch, instructionSearchCmd) = InstructionSearch.State.update msg model.InstructionSearch
        { model with InstructionSearch = instructionSearch }, Cmd.map InstructionSearchMsg instructionSearchCmd
    | CategorySearchMsg msg ->
        let (category, categoryCmd) = Category.State.update msg model.Category
        { model with Category = category }, Cmd.map CategorySearchMsg categoryCmd 
