module App.State

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Browser
open Global
open Types

let pageParser: Parser<Page->Page,Page> =
    oneOf [
        map Part (s "part")
        map Instruction (s "instruction")
        map InstructionSearch (s "search")
    ]

let urlUpdate (result : Page option) model =
    match result with
    | None ->
        console.error("Error parsing url")
        model, Navigation.modifyUrl (toHash model.CurrentPage)
    | Some page ->
        { model with CurrentPage = page }, []

let init result =

      let currPage = InstructionSearch
      let (instructionSearch, instructionSearchCmd) =  InstructionSearch.State.init()
      let (instruction, instructionCmd) = Instruction.State.init()
      let (part, partCmd) = Part.State.init()
      let (model, cmd) =
        urlUpdate result
            {
                CurrentPage = currPage
                InstructionSearch = instructionSearch
                Instruction = instruction
                Part = part
            }

      model, Cmd.batch [ cmd
                         Cmd.map PartMsg partCmd
                         Cmd.map InstructionSearchMsg instructionSearchCmd
                         Cmd.map InstructionMsg instructionCmd]

let update msg model =
    match msg with
    | PartMsg msg ->
        let (part, partCmd) = Part.State.update msg model.Part
        {model with Part = part}, Cmd.map PartMsg partCmd
    | InstructionSearchMsg msg ->
        let (instructionSearch, instructionSearchCmd) = InstructionSearch.State.update msg model.InstructionSearch
        {model with InstructionSearch = instructionSearch}, Cmd.map InstructionSearchMsg instructionSearchCmd
    | InstructionMsg msg ->
        let (instruction, instructionCmd) = Instruction.State.update msg model.Instruction
        {model with Instruction = instruction}, Cmd.map InstructionMsg instructionCmd
