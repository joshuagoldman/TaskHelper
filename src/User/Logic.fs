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
open Elmish

let go2PartOrInstruction dispatch result =
    match result with
    | InstructionSearch.Types.Part (partModel, _, instruction, _) ->
        Part.State.NewPart2Show (partModel,instruction)
        |> Instruction.State.PartMsg
        |> User.Types.InstructionMsg
        |> dispatch
        |> fun _ ->
            Part.Logic.go2PreviousOrNext instruction partModel.Title (Instruction.State.PartMsg >>
                                                                      User.Types.InstructionMsg >>
                                                                      dispatch) ""
        
            
            
    | InstructionSearch.Types.Instruction (instruction, _) ->
        Instruction.State.NewInstruction2Show instruction
        |> User.Types.InstructionMsg
        |> dispatch

let WritePartOrInstruction result =
    match result with
    | InstructionSearch.Types.Part (partData, _, _, _) -> partData.Title
    | InstructionSearch.Types.Instruction (instruction, _) -> instruction.Title

let choosePage page =
    match page with
    | InstructionSearch.Types.Part (_, _, _, _) ->
                Global.Part
    | InstructionSearch.Types.Instruction (_,_) ->
                Global.Instruction

let searchInfo info (keyWord : string) =
    match info with
    | InstructionSearch.Types.Instruction (instruction, _) -> instruction.Title.ToLower().Contains keyWord && keyWord <> ""
    | InstructionSearch.Types.Part (partData, _, _, _) -> partData.Title.ToLower().Contains keyWord && keyWord <> ""

let loadInitData data =

    let initInstruction =
        data.Instructions |> Seq.item 0

    let initPart =
        data.Instructions
        |> Seq.item 0
        |> fun x -> x.Data
        |> Seq.item 0
    seq
        [
            (initInstruction |>
             (Instruction.State.NewInstruction2Show >> User.Types.InstructionMsg))
            ((initPart,initInstruction) |>
             (Part.State.NewPart2Show >>
              Instruction.State.PartMsg >>
              User.Types.InstructionMsg))
        ]

                       


let loadData ( status : Data.Deferred<Result<UserData,string>> ) =
    match status with
    | HasNostStartedYet -> "No data has been loaded to user" |> Error
        
    | InProgress -> "Data is still loading..." |> Error
        
    | Resolved response ->
        match response with
        | Ok result -> Ok result                
        | Error err -> err |> Error

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

let saveInto (info : {| Data : FormData ; CntType : string ; Path : string ; Name : string |}) = async{
        do! Async.Sleep 10000
        let! response = 
            Http.request ("http://localhost:8081/" + info.Path)
            |> Http.method POST
            |> Http.content (BodyContent.Form info.Data)
            |> Http.header (Headers.contentType info.CntType)
            |> Http.send

        match response.statusCode with
        | 200 ->
            return (
                info.Name + " was succesfully loaded"
                |> NewAdd.Types.NewAddInfoMsg |> User.Types.NewAddMsg
            )
        | _ ->
            return (
                "saving file " +
                 info.Name +
                 " failed with status code: "
                 + (response.statusCode |> string)
                 |> NewAdd.Types.NewAddInfoMsg |> User.Types.NewAddMsg
            )
    }

let shiputsnik formDt name filetype =
    fileType = ""
    |> Seq.exists (fun fileType -> fileType = "" || fileType = "")
    |> function
        | res when res = true ->
                Seq.zip fileTypes names
                |> Seq.filter (fun (fileType,_) -> fileType <> "" || fileType <> "")
                |> Seq.map (fun (_,name) ->
                                        "file " +
                                        name +
                                        " and")
                |> function
                    | res when (res |> Seq.length > 1) ->
                        res
                        |> String.concat "ssss"
                        |> fun x -> x.Substring (0, x.LastIndexOf("and"))
                        |> fun x -> (x + "have forbidden format")
                        |> NewAdd.Types.NewAddInfoMsg |> User.Types.NewAddMsg
                        |> fun x ->
                                Cmd.batch
                                    (seq[x]
                                    |> Seq.map (fun msg -> Cmd.ofMsg msg))
                    | res when (res |> Seq.length = 1) ->
                        res
                        |> String.concat "ssss"
                        |> fun x -> x.Substring (0, x.LastIndexOf("and"))
                        |> fun x -> (x + "has a forbidden format")
                        |> NewAdd.Types.NewAddInfoMsg |> User.Types.NewAddMsg
                        |> fun x ->
                                Cmd.batch
                                    (seq[x]
                                    |> Seq.map (fun msg -> Cmd.ofMsg msg))
                    | _ -> 
                            "I dunno wat kind of error dis is man"
                            |> NewAdd.Types.NewAddInfoMsg |> User.Types.NewAddMsg
                            |> fun x ->
                                    Cmd.batch
                                        (seq[x]
                                        |> Seq.map (fun msg -> Cmd.ofMsg msg))
                                    

        | _ ->
            let postInfo =
                Seq.zip formDt [0..formDt |> Seq.length |> fun x -> x - 1]
                |> Seq.map (fun (data, pos) -> {|
                                                    Data = data
                                                    CntType = cntTypes |> Seq.item pos
                                                    Path = paths |> Seq.item pos
                                                    Name = names |> Seq.item pos
                                                |})
            
            postInfo
            |> Seq.map (fun info -> saveInto info)
            |> fun x -> Cmd.batch
                            ( x
                              |> Seq.map (fun msgAsync -> Cmd.fromAsync msgAsync)
                            )

let saveUserData formDt names fileTypes = 
    let cntTypes = seq["application/" ; ""]
    let paths = seq["Instructions" ; "Videos"]

    let formDtExtract =
        formDt
        |> Seq.map (fun data ->
            match data with
            | NewAdd.Types.Video videoFormData -> videoFormData
            | NewAdd.Types.InstructionTxt instructionFormData -> instructionFormData)

    fileTypes
    |> Seq.exists (fun fileType -> fileType = "" || fileType = "")
    |> function
        | res when res = true ->
                Seq.zip fileTypes names
                |> Seq.filter (fun (fileType,_) -> fileType <> "" || fileType <> "")
                |> Seq.map (fun (_,name) ->
                                        "file " +
                                        name +
                                        " and")
                |> function
                    | res when (res |> Seq.length > 1) ->
                        res
                        |> String.concat "ssss"
                        |> fun x -> x.Substring (0, x.LastIndexOf("and"))
                        |> fun x -> (x + "have forbidden format")
                        |> NewAdd.Types.NewAddInfoMsg |> User.Types.NewAddMsg
                        |> fun x ->
                                Cmd.batch
                                    (seq[x]
                                    |> Seq.map (fun msg -> Cmd.ofMsg msg))
                    | res when (res |> Seq.length = 1) ->
                        res
                        |> String.concat "ssss"
                        |> fun x -> x.Substring (0, x.LastIndexOf("and"))
                        |> fun x -> (x + "has a forbidden format")
                        |> NewAdd.Types.NewAddInfoMsg |> User.Types.NewAddMsg
                        |> fun x ->
                                Cmd.batch
                                    (seq[x]
                                    |> Seq.map (fun msg -> Cmd.ofMsg msg))
                    | _ -> 
                            "I dunno wat kind of error dis is man"
                            |> NewAdd.Types.NewAddInfoMsg |> User.Types.NewAddMsg
                            |> fun x ->
                                    Cmd.batch
                                        (seq[x]
                                        |> Seq.map (fun msg -> Cmd.ofMsg msg))
                                    

        | _ ->
            let postInfo =
                Seq.zip formDt [0..formDt |> Seq.length |> fun x -> x - 1]
                |> Seq.map (fun (data, pos) -> {|
                                                    Data = data
                                                    CntType = cntTypes |> Seq.item pos
                                                    Path = paths |> Seq.item pos
                                                    Name = names |> Seq.item pos
                                                |})
            
            postInfo
            |> Seq.map (fun info -> saveInto info)
            |> fun x -> Cmd.batch
                            ( x
                              |> Seq.map (fun msgAsync -> Cmd.fromAsync msgAsync)
                            )

let SaveNewInstruction ( model : NewAdd.Types.Model )
                       ( status : Data.Deferred<Result<Data.InstructionData,string>> ) =
    match status with
    | HasNostStartedYet ->
        seq[(NewAdd.Types.CreateNewDataMsg Started |> User.Types.NewAddMsg)]
        
        
    | InProgress ->
        seq["Sending files to server..." |> ( NewAdd.Types.NewAddInfoMsg >> User.Types.NewAddMsg)]
        
    | Resolved response ->
        match response with
        | Ok result ->
            result.Data
            |> 
        | Error err -> seq[err |> ( NewAdd.Types.NewAddInfoMsg >> User.Types.NewAddMsg)]

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
    | Data.HasNostStartedYet -> seq["" |> User.Types.LoginMessages] 
    | Data.InProgress -> seq["Loading User Data" |> User.Types.LoginMessages] 
    | Data.Resolved response ->
        match response with
        | Ok result ->
            let successMessage =
                seq
                    [
                        ("received query with " +
                         (result.Instructions |> Seq.length |> string) +
                         " instructions :)" |> User.Types.LoginMessages)
                    ]
            successMessage
            |> Seq.append (loadInitData result)

            

        | Error err ->seq[ err |> User.Types.LoginMessages]

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
    | HasNostStartedYet ->
        seq[User.Types.LoadedUsers Started]
        
        
    | InProgress ->
        seq["loading all users" |> User.Types.LoginMessages]
        
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
                                        usernameMatchExists.Value.Id |> (Part.Types.NewUserIdMsg >>
                                                                         Instruction.Types.PartMsg >>
                                                                         User.Types.InstructionMsg) 
                                        User.Types.LoadedInstructions Started
                                    ]
                           | _ -> seq["Wrong password for the given user name" |> LoginMessages]
                    | _ -> seq[ "The password provided is invalid" |> LoginMessages]
               | _ -> seq["The user name provided is invalid" |> LoginMessages]
                        
                        
                        
        | Error err -> seq[err|> User.Types.LoginMessages]

let validateLoginInfo info =
    info
    |> function
       | _ when info = "" ->
            Invalid
       | _ ->
            Valid info

