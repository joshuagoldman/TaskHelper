module User.Logic

open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Feliz
open Types
open System
open Data
open InstructionSearch.Types
open Browser
open Global
open Data
open Types
open Fable.React
open Browser
open Thoth.Json
open Fable.SimpleHttp

let go2PartOrInstruction dispatch result =
    match result with
    | InstructionSearch.Types.Part (partModel, _) ->
        Part.State.NewPart2Show partModel
        |> Instruction.State.PartMsg
        |> User.Types.InstructionMsg
        |> dispatch
        |> fun _ -> Part.Logic.go2PreviousOrNext partModel (Instruction.State.PartMsg >>
                                                            User.Types.InstructionMsg >>
                                                            dispatch) "" 
    | InstructionSearch.Types.Instruction (instructionModel, _) ->
        Instruction.State.NewInstruction2Show instructionModel
        |> User.Types.InstructionMsg
        |> dispatch

let WritePartOrInstruction result =
    match result with
    | InstructionSearch.Types.Part (instruction, _) -> instruction.Title
    | InstructionSearch.Types.Instruction (data, _) -> data.Title

let choosePage page =
    match page with
    | InstructionSearch.Types.Part (_,_) -> Global.Part
    | InstructionSearch.Types.Instruction (_,_) -> Global.Instruction

let searchInfo info (keyWord : string) =
    match info with
    | InstructionSearch.Types.Instruction (model, _) -> model.Title.ToLower().Contains keyWord && keyWord <> ""
    | InstructionSearch.Types.Part (model, _) -> model.Title.ToLower().Contains keyWord && keyWord <> ""

let instructionResults =
    allData ""
    |> Seq.map (fun instruction -> InstructionSearch.Types.Instruction(instruction, []))
let partResults =
    allData ""
    |> Seq.collect (fun instruction -> instruction.Data
                                       |> Seq.map (fun part -> InstructionSearch.Types.Part(part, [])))


let initInstruction =
    seq
        [
            allData "" |> Seq.item 0
        ]

let jsonDecodingInstructions jsonString =
    let decodingObj = Data.parseUserData jsonString

    match decodingObj with
    | Ok result ->
        LoadedInstructions (Finished (Ok (result.[0])))
    | Error result ->
        LoadedInstructions (Finished (Error result))

let jsonDecodingUsers jsonString =
    let decodingObj = Data.LoginInfoArrayDecoder jsonString

    match decodingObj with
    | Ok result ->
        result
        |> Array.map (fun o -> { Username = Exists o.Username ; Password = Exists o.Password} : LoginInfo )
        |> fun arr -> arr |> Array.toSeq |> (Ok >> Finished >> LoadedUsers)
    | Error result ->
        LoadedUsers (Finished (Error result))


let loadInstructionItems = async {
        do! Async.Sleep 3000
        let! response = 
            Http.request "http://localhost:3001/api/instructions"
            |> Http.method GET
            |> Http.header (Headers.contentType "application/json")
            |> Http.send
        match response.statusCode with
        | 200 ->
            return jsonDecodingInstructions (response.responseText
                                            |> fun x -> x.Replace( """[{"array_to_json":""", "")
                                            |> fun x -> x.Substring(0, x.LastIndexOf("}]")))
            
        | _ ->
            return LoadedInstructions (Finished (Error ("Could not get api, status code: " +
                                                        (response.statusCode |> string))))  
    }

let loadUserItems = async {
    do! Async.Sleep 3000
    let! response = 
        Http.request "http://localhost:3001/api/users"
        |> Http.method GET
        |> Http.header (Headers.contentType "application/json")
        |> Http.send
    match response.statusCode with
    | 200 ->
        return jsonDecodingUsers (response.responseText)
        
    | _ ->
        return LoadedInstructions (Finished (Error ("Could not get api, status code: " +
                                                    (response.statusCode |> string))))  
}

