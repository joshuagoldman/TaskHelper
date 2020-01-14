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
open Data

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

let getUserData result ( model : User.Types.Model ) =
    result
    |> Seq.tryFind (fun user -> user.Id = model.Id  )
    |> function
       | res when res <> None -> res.Value
       | _ ->
            {
                Id = 0
                Instructions =
                    seq
                        [
                            {
                                Title = ""
                                Data =
                                    seq
                                        [
                                            {
                                                InstructionTxt = ""
                                                InstructionVideo = ""
                                                Title = ""
                                            }
                                        ]
                            }
                        ]

            }

let jsonDecodingInstructions jsonString model =
    let decodingObj = parseUserData jsonString

    match decodingObj with
    | Ok result ->
        LoadedInstructions (Finished (Ok (getUserData result model)))
    | Error result ->
        LoadedInstructions (Finished (Error result))

let jsonDecodingUsers jsonString =
    let decodingObj = LoginInfoArrayDecoder jsonString

    match decodingObj with
    | Ok result ->
        result
        |> Array.map (fun o -> { Username = o.Username ; Password = o.Password ; Id = o.Id} : LoginInfo )
        |> fun arr -> arr |> Array.toSeq |> (Ok >> Finished >> LoadedUsers)
    | Error result ->
        LoadedUsers (Finished (Error result))


let loadInstructionItems model = async {
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
                                            |> fun x -> x.Substring(0, x.LastIndexOf("}]"))) model
            
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
        return LoadedUsers (Finished (Error ("Could not get api, status code: " +
                                                    (response.statusCode |> string))))
}

let getUserDataUpdate ( userData : Data.Deferred<Result<Data.UserData, string>> ) =
    match userData with
    | Data.HasNostStartedYet -> "" |> User.Types.LoginMessages 
    | Data.InProgress -> "Loading User Data" |> User.Types.LoginMessages 
    | Data.Resolved response ->
        match response with
        | Ok result ->
            "received query with " +
            (result.Instructions |> Seq.length |> string) +
            " instructions :)" |> User.Types.LoginMessages

        | Error err -> err |> User.Types.LoginMessages


let message4Dispatch value =
    value |> LoginMessages

let existOrNot compareVal result =
     match compareVal with
     | Valid str ->
         result
         |> Seq.tryFind (fun usrName -> usrName.Username = str)
         |> function
            | res when res <> None ->
                     Some res.Value
            | _ -> None
     | Invalid ->
         None

let loginAttempt ( model : User.Types.Model ) ( status : Data.Deferred<Result<seq<LoginInfo>, string>> ) =
    match status with
    | HasNostStartedYet -> seq[User.Types.LoadedUsers Started]
        
    | InProgress -> seq["loading all users" |> User.Types.LoginMessages]
        
    | Resolved response ->
        match response with
        | Ok result ->
            let usernameMatchExists = existOrNot model.UserFromLogin.Username result
              
            ()
            |> function
               | _ when usernameMatchExists <> None ->
                    match model.UserFromLogin.Password with
                    | Valid password ->
                        password
                        |> function
                           | _ when password = usernameMatchExists.Value.Password ->
                                seq
                                    [
                                        usernameMatchExists.Value.Id |> NewUserId
                                        User.Types.LoadedInstructions Started
                                    ]
                           | _ -> seq["Wrong password for the given user name" |> LoginMessages]
                    | _ ->seq[ "The password provided is invalid" |> LoginMessages]
               | _ -> seq["The user name provided is invalid" |> LoginMessages]
                        
                        
                        
        | Error err -> seq[err|> User.Types.LoginMessages]

