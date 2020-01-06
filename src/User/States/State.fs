module User.State

open Elmish
open Elmish.Navigation
open Fable.SimpleHttp
open Browser
open Global
open Data
open Types
open Fable.React
open Browser

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

let jsonDecoding jsonString =
    let decodingObj = Data.parseInstructionItems jsonString

    match decodingObj with
    | Ok result ->
        LoadedInstructions (Finished (Ok (result |> List.toSeq)))
    | Error result ->
        LoadedInstructions (Finished (Error result))

let loadInstructionItems = async {
        do! Async.Sleep 3000
        let! response = 
            Http.request "http://localhost:3001/api/instructions"
            |> Http.method GET
            |> Http.header (Headers.contentType "application/json")
            |> Http.send
        match response.statusCode with
        | 200 ->
            return jsonDecoding response.responseText
            
        | _ ->
            return LoadedInstructions (Finished (Error ("Could not get api, status code: " +
                                                        (response.statusCode |> string))))  
    }

let init() : Model * Cmd<Msg> =
    {
        Username = ""
        Password = ""
        CurrentPage = InstructionSearch
        InstructionSearch = InstructionSearch.State.init() |> fun (a,_) -> a
        Category = Category.State.init() |> fun (a,_) -> a
        UserData = Data.HasNostStartedYet
        NewAdd = NewAdd.State.init() |> fun(a,_) -> a
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
    | NewAddMsg msg ->
        let (newAdd, newAddCmd) = NewAdd.State.update msg model.NewAdd
        { model with NewAdd = newAdd }, Cmd.map NewAddMsg newAddCmd
