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
        Part.Types.NewPart2Show (partModel,instruction)
        |> Instruction.Types.PartMsg
        |> User.Types.InstructionMsg
        |> dispatch
        |> fun _ ->
            Part.Logic.go2PreviousOrNext instruction partModel.Title (Instruction.Types.PartMsg >>
                                                                      User.Types.InstructionMsg >>
                                                                      dispatch) ""
        
            
            
    | InstructionSearch.Types.Instruction (instruction, _) ->
        Instruction.Types.NewInstruction2Show instruction
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
             (Instruction.Types.NewInstruction2Show >> User.Types.InstructionMsg))
            ((initPart,initInstruction) |>
             (Part.Types.NewPart2Show >>
              Instruction.Types.PartMsg >>
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

type PostInstructionInfo =
    abstract retrieveAll : unit -> string
    abstract insert : string -> string

let instructionToSql ( ids : string ) instruction =
    let id =
        ids.Substring(0,ids.LastIndexOf("_"))

    let instructionId =
        ids.Substring(ids.LastIndexOf("_"), ids |> String.length |> fun len-> len - 1)

    let sqlInstructionVars =
        seq[
            id
            instructionId
            instruction.Title
        ]
    let instructionInsert =
        String.Format(
            "INSERT INTO instructions ( id, instruction_id, title )
             VALUES ( {0}, {1}, {2});\n", sqlInstructionVars )

    let partInsert =
        instruction.Data
        |> Seq.map (fun part ->
              String.Format(
                "INSERT INTO parts ( instruction_id, instruction_video, instruction_txt, part_title)
                VALUES ( {0}, {1}, {2}, {3});\n",
                seq[instructionId ; part.InstructionVideo ; part.InstructionTxt ; part.Title]
              ))
        |> String.concat ""

    instructionInsert + partInsert

let postInstructionToDatabase ( status : Result<Data.InstructionData,string> ) ids =

    let postObj = importAll<PostInstructionInfo> "../server/model/instructions"

    let insertInstructionAsync sqlCommand = async{
        let response = postObj.insert sqlCommand
        do! Async.Sleep 4000
        return(
            response
            |> NewAdd.Types.NewAddInfoMsg
            |> Cmd.ofMsg
        )
    }
    
    match status with
    | Ok result ->
       let sqlCommand = instructionToSql ids result
       Async.StartAsTask( insertInstructionAsync sqlCommand).Result


    | Error err ->
        err
        |> NewAdd.Types.NewAddInfoMsg
        |> Cmd.ofMsg

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
                   |> NewAdd.Types.NewAddInfoMsg
                   |> Error
                
            )
        | _ ->
            return (
                    "saving file " +
                     info.Name +
                     " failed with status code: "
                     + (response.statusCode |> string)
                     |> NewAdd.Types.NewAddInfoMsg
                     |> Ok
            )
    }

let createInstructionFromFile ( files : seq<NewAdd.Types.MediaChoiceFormData>) idString =


    match idString with
    | None ->
        seq[
            "" |> (NewAdd.Types.NewAddInfoMsg >> User.Types.NewAddMsg)
        ]
        |> Seq.map (fun msg -> Cmd.ofMsg msg)
        |> Cmd.batch
    | Some id ->
        let mutable videosSequence = seq[]
        let mutable instructionSequence = seq[]

        files
        |> Seq.iter (fun mediaContent ->
                            match mediaContent with
                            | NewAdd.Types.Video f ->
                                Seq.append videosSequence [f]
                                |> ignore
                            | NewAdd.Types.InstructionTxt f ->
                                Seq.append instructionSequence [f]
                                |> ignore)


        Seq.zip videosSequence instructionSequence
        |> Seq.map (fun (video,txt) ->
                    {
                        Title = ""
                        InstructionVideo = id + video.name
                        InstructionTxt = id + txt.name
                    })
        |> fun parts ->
                {
                    Title = ""
                    Data = parts
                }
        |> Instruction.Types.NewInstruction2Show
        |> User.Types.InstructionMsg
        |> fun x -> seq[x]
                    |> Seq.map (fun msg -> Cmd.ofMsg msg)
                    |> Cmd.batch

let checkfileTypeAndSave ( file : Types.File ) validType path =
    validType
    |> function
        | _ when file.``type`` = validType ->
            let fileInfo =
                {| Data = (FormData.Create() |> fun frmData -> ("resume",file)
                                                                |> frmData.append
                                                                |> fun _ -> frmData)
                   CntType = file.``type``
                   Path = path
                   Name = file.name
                |}
            Async.StartAsTask(saveInto fileInfo).Result
            |> fun res ->
                match res with
                | Ok msg ->
                    msg
                    |> User.Types.NewAddMsg
                    |> fun x ->
                        Cmd.batch[
                            Cmd.ofMsg x
                        ]
                        |> Ok 
                    
                | Error msg ->
                    msg
                    |> User.Types.NewAddMsg
                    |> fun x ->
                        Cmd.batch[
                            Cmd.ofMsg x
                            ]
                        |> Error
            
        | _ ->
            ("file " +
             file.name +
             " is of type: " +
             file.``type`` +
             " which is invalid!")
            |> NewAdd.Types.NewAddInfoMsg
            |> User.Types.NewAddMsg
            |> fun msg -> Cmd.batch[
                            Cmd.ofMsg msg
                          ]
                          |> Error

let saveUserData file =
    let addPostMessageIfSuccess res =
        match res with
        | Ok msg ->
              Cmd.batch[
                  msg
                  Cmd.ofMsg(
                      file
                      |> (NewAdd.Types.PostInstruction >>
                          User.Types.NewAddMsg)
                  )
                ]
        | Error msg ->
            Cmd.batch[
                msg
              ]

    let formDtExtract =
        file
        |> Seq.map (fun data ->
            match data with
            | NewAdd.Types.Video videoFormData ->
                    checkfileTypeAndSave videoFormData "" "Videos"
                    |> addPostMessageIfSuccess
                                        
            | NewAdd.Types.InstructionTxt instructionFormData ->
                    checkfileTypeAndSave instructionFormData "" "Instructions"
                    |> addPostMessageIfSuccess )

    formDtExtract

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

let createNewInstructionId id userData =
    userData.Instructions
    |> Seq.length
    |> fun len -> (id |> string) + "_" + (len + 1 |> string)
    |> NewAdd.Types.NewInstructionIdMsg
    |> User.Types.NewAddMsg
    |> Cmd.ofMsg

