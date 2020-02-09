module User.Logic

open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
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
open Browser
open Feliz

let divWithStyle msg properties =
    Html.div[
        prop.className "column"
        properties
        prop.children[
            Html.br[]
            str msg
        ]
    ]

let spinner =
    Html.div[
        prop.className "column"
        prop.style[
        ]
        prop.children[
            Html.i[
                prop.className "fa fa-cog fa-spin fa-2x"
            ]
        ]
    ]

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

type PostInstructionInfo =
    abstract insert : string -> string -> string

//[<Import("*", "../../server/model/instructions")>]
//let postObj : PostInstructionInfo = jsNative

let postInstructionToDatabase ( status : Result<Data.InstructionData,string> ) ids =
 

    let insertInstructionAsync sqlCommand = async{
        //let response = postObj.insert sqlCommand ""
        do! Async.Sleep 4000
        //return(
        //    response
        //    |> NewAdd.Types.NewAddInfoMsg
        //    |> Cmd.ofMsg
        return (seq[str "Loading User Data"] |> NewAdd.Types.NewAddInfoMsg |> Cmd.ofMsg)
        
    }
    
    match status with
    | Ok result ->
       let sqlCommand = instructionToSql ids result
       ( insertInstructionAsync sqlCommand)
       |> Async.RunSynchronously


    | Error err ->
        seq[str err]
        |> NewAdd.Types.NewAddInfoMsg
        |> Cmd.ofMsg

let createInstructionFromFile ( files : seq<NewAdd.Types.MediaChoiceFormData>) idString =


    match idString with
    | None ->
        seq[str ""] |> (NewAdd.Types.NewAddInfoMsg >> User.Types.NewAddMsg)
        |> Cmd.ofMsg 
    | Some id ->
        let mutable videosSequence = seq[]
        let mutable instructionSequence = seq[]

        files
        |> Seq.iter (fun mediaContent ->
                            match mediaContent with
                            | NewAdd.Types.Video (vid,_) ->
                                Seq.append videosSequence [vid]
                                |> ignore
                            | NewAdd.Types.InstructionTxt (instrctn,_) ->
                                Seq.append instructionSequence [instrctn]
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

let saveAsync ( fileInfo : (Types.File * string) )
              ( media : NewAdd.Types.MediaChoiceFormData ) = async{
    let file = fileInfo |> fun (x,_) -> x

    let root = fileInfo |> fun (_,x) -> x

    console.log(root)
    console.log("starting to post")
    let! response =
        Http.request ("http://localhost:8081/" + root)
        |> Http.method POST
        |> Http.content (BodyContent.Binary file)
        |> Http.send

    console.log("post ended")

    match response.statusCode with
    | 200 ->
        return (
            divWithStyle
                ("file was succesfully loaded")
                ( prop.style[ style.color.green ; style.fontWeight.bold ] )
            |> fun x -> (media, NewAdd.Types.IsUploading.No(x))
                        |> ( NewAdd.Types.ChangeFileStatus >> User.Types.NewAddMsg )
            
        )
    | _ ->
        return (
                divWithStyle
                    ("file failed with status code: " +
                     ( response.statusCode |> string ))
                     ( prop.style[ style.color.red ; style.fontWeight.bold ] )
                |> fun x -> (media, NewAdd.Types.IsUploading.No(x))
                            |> ( NewAdd.Types.ChangeFileStatus >> User.Types.NewAddMsg )
        )
}

let matchMediaBeforeSave media =
    let fileSavingInfo =
        match media with
        | NewAdd.Types.Video (vid,_) ->
            (vid, "/Videos/")
        | NewAdd.Types.InstructionTxt (instrctn,_) ->
            (instrctn,"/Instructions/")

    saveAsync fileSavingInfo media

let saveUserData ( status : SaveDataProgress<NewAdd.Types.MediaChoiceFormData> ) =
    match status with 
    | SavingHasNostStartedYet media ->
        let msg =
            divWithStyle
                "File is uploading"
                (prop.style[style.color.black ; style.fontWeight.bold] )
        (media,NewAdd.Types.IsUploading.Yes(msg))
        |> ( NewAdd.Types.ChangeFileStatus >> User.Types.NewAddMsg )
        |> fun x ->
            seq[
                x
                media |> ( SavingInProgress >>
                           SavingOnGoing >>
                           NewAdd.Types.CreateNewDataMsg >>
                           User.Types.NewAddMsg)
            ]
            |> Seq.map (fun msg -> msg |> Cmd.ofMsg )

    | SavingInProgress media ->
        matchMediaBeforeSave media
        |> Cmd.fromAsync
        |> fun x -> seq[x]


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

let chooseMediaByName name file =
    name
    |> function
        | res when res = "videos" ->
            NewAdd.Types.Video (file,NewAdd.Types.IsUploading.No Html.none)
        | res when res = "instructions" ->
            NewAdd.Types.InstructionTxt (file,NewAdd.Types.IsUploading.No Html.none)
        | _ -> NewAdd.Types.Video (file,NewAdd.Types.IsUploading.No Html.none)

let isuploading result =
    match result with
    | NewAdd.Types.IsUploading.Yes msg ->
        seq[
            spinner
            msg
        ]
    | NewAdd.Types.IsUploading.No msg ->
        msg
        |> fun x -> seq[x]
        

let filenameWStatus file =
    match file with
    | NewAdd.Types.Video (vid,status) ->
        Html.div[
            prop.className "columns is-centered"
            prop.children(
               isuploading status
               |> Seq.append(
                Html.div[
                    prop.className "column"
                    prop.children[
                        str vid.name
                    ]
                ]
                |> fun x -> seq[x]
               )
            )
        ]

    | NewAdd.Types.InstructionTxt (instrctn,status) -> 
        Html.div[
            prop.className "columns is-centered"
            prop.children(
               isuploading status
               |> Seq.append(
                Html.div[
                    prop.className "column"
                    prop.children[
                        str instrctn.name
                    ]
                ]
                |> fun x -> seq[x]
               )
            )
        ]

let extractFileNames files name =
    files
    |> Seq.filter (fun media ->
        name
        |> function
            | res when res = "videos" ->
                match media with
                | NewAdd.Types.Video (_,_) -> true
                | NewAdd.Types.InstructionTxt (_,_) -> false
            | _ ->
                match media with
                | NewAdd.Types.Video (_,_) -> false
                | NewAdd.Types.InstructionTxt (_,_) -> true)
    |> Seq.map (fun file -> filenameWStatus file)
    |> fun elements ->
        Seq.append [ str "Files chosen are:" ; Html.br[] ; Html.br[]] elements   

let currFilesInfo ( filesOption : Option<seq<NewAdd.Types.MediaChoiceFormData>> )
                    name =

    match filesOption with
    | Some files ->
        files
        |> Seq.filter (fun media ->
            name
            |> function
                | res when res = "videos" ->
                    match media with
                    | NewAdd.Types.Video (_,_) -> true
                    | NewAdd.Types.InstructionTxt (_,_) -> false
                | _ ->
                    match media with
                    | NewAdd.Types.Video (_,_) -> false
                    | NewAdd.Types.InstructionTxt (_,_) -> true)
        |> function
            | res when (res |> Seq.length) = 0 ->
                seq[Html.div[prop.children[str "No files chosen"]]]
            | _ -> extractFileNames files name
    | None -> seq[Html.div[prop.children[str "No files chosen"]]]

let infoText ( model : NewAdd.Types.Model ) dispatch name =
    Html.div[
        prop.style[
            style.color.black
            style.fontStyle.italic
            style.fontWeight.bold
            style.textAlign.center
        ]
        prop.children(
            currFilesInfo model.NewInstructionData name
        )
    ]

let removeOldOfSame ( medias : seq<NewAdd.Types.MediaChoiceFormData> )
                      name =
    name
    |> function
        | res when res = "videos" ->
            medias
            |> Seq.filter (fun media ->
                match media with
                | NewAdd.Types.Video _ ->
                    false
                | NewAdd.Types.InstructionTxt _ ->
                    true)
        | _ ->
            medias
            |> Seq.filter (fun media ->
                match media with
                | NewAdd.Types.Video _ ->
                    true
                | NewAdd.Types.InstructionTxt _ ->
                    false)


let extractMedia ( medias : Option<seq<NewAdd.Types.MediaChoiceFormData>> )
                 ( newVids : seq<NewAdd.Types.MediaChoiceFormData> )
                   name=
    match medias with
    | Some vids->
        newVids
        |> Seq.append (removeOldOfSame vids name)
    | None -> newVids

let decideIfRightFormat ( medias : seq<NewAdd.Types.MediaChoiceFormData>) =
    medias
    |> Seq.filter (fun media ->
        match media with
        | NewAdd.Types.Video (vid,_) ->
            vid.``type`` <> "video/mpeg" &&
            vid.``type`` <> "video/ogg" &&
            vid.``type`` <> "video/mp4" &&
            vid.``type`` <> "video/avi"

        | NewAdd.Types.InstructionTxt (instrctn,_) ->
            instrctn.``type`` <> "text/plain")
    |> function
        | res when ( res |> Seq.length ) = 0 ->
            None
        | res ->
            let initialMessage =
                seq[
                    divWithStyle
                        "The following files did not have the the right file type:"
                        (prop.style[style.color.black ; style.fontWeight.bold] )
                ]
            let secondMessage = 
                res
                |> Seq.map (fun media ->
                    match media with
                    | NewAdd.Types.Video (vid,_) ->
                        seq[
                            divWithStyle
                                vid.name
                                (prop.style[style.color.indianRed ; style.fontWeight.bold] )
                        ]

                    | NewAdd.Types.InstructionTxt (instrctn,_) ->
                        seq[
                            divWithStyle
                                instrctn.name
                                (prop.style[style.color.indianRed ; style.fontWeight.bold] )
                        ])
                |> Seq.collect (fun components -> components)
            let finalMessage =
                seq[
                    divWithStyle
                        "Allowed video formats are:"
                        (prop.style[style.color.black ; style.fontWeight.bold] )
                    divWithStyle
                        ".mpeg, .mp4, .ogg, .avi."
                        (prop.style[style.color.indianRed ; style.fontWeight.bold] )
                    divWithStyle
                        "For instruction text files:"
                        (prop.style[style.color.black ; style.fontWeight.bold] )
                    divWithStyle
                        ".md (Markdown)"
                        (prop.style[style.color.indianRed ; style.fontWeight.bold] )
                ]

            finalMessage
            |> Seq.append secondMessage
            |> Seq.append initialMessage
            |> Some
                 

let decideIfUploadableByTypeCount ( medias : seq<NewAdd.Types.MediaChoiceFormData>) =
    let mutable videos = seq[]
    let mutable instructions = seq[]
    medias
    |> Seq.iter (fun media ->
        match media with
        | NewAdd.Types.Video (vid,_) ->
            videos <- videos |> Seq.append [vid]
        | NewAdd.Types.InstructionTxt (instrctn,_) ->
            instructions <- instructions |> Seq.append [instrctn])

    ()
    |> function
        | _ when ( videos |> Seq.length ) = ( instructions |> Seq.length ) ->
            None
        | _ ->
            seq[
                divWithStyle
                    "צריך לבחור אותו כמות של קבצי וידאו ומארקדבן"
                    (prop.style[style.color.blueViolet ; style.fontWeight.bold])
            ]
            |> Some 

let decideIfUploadValid ( medias : seq<NewAdd.Types.MediaChoiceFormData>)
                        ( model : NewAdd.Types.Model )
                          dispatch =

    seq[decideIfUploadableByTypeCount medias]
    |> Seq.append [decideIfRightFormat medias]
    |> Seq.filter (fun msgs ->
        match msgs with
        | Some _ -> true
        | None -> false)
    |> function
        | res when ( res |> Seq.length = 0 ) ->
            seq[
                divWithStyle
                    "Data will now be saved"
                    (prop.style[style.color.indianRed ; style.fontWeight.bold])
            ]
            |> NewAdd.Types.NewAddInfoMsg
            |> fun x -> (medias
                         |> Seq.map (fun media -> media |> 
                                                   (SavingHasNostStartedYet >>
                                                    SavingWillBegin >>
                                                    NewAdd.Types.CreateNewDataMsg))
                        |> fun y -> Seq.append [x] y
                        |> Seq.iter (fun msg -> (msg |> dispatch)))
                        
        | res ->
            res
            |> Seq.collect (fun msgs -> msgs.Value)
            |> ( NewAdd.Types.NewAddInfoMsg >> dispatch )
  

let isUploadable ( model : NewAdd.Types.Model )
                   dispatch =
    match model.NewInstructionData with
    | Some res ->
        res
        |> function
            | _ when res |> Seq.length = 0 ->
                seq[
                    divWithStyle
                        "לא היה הבחרת קביצה"
                        (prop.style[style.color.indianRed ; style.fontWeight.bold])
                ]
                |> ( NewAdd.Types.NewAddInfoMsg >> dispatch )
            | _ ->
                decideIfUploadValid res model dispatch
    | None ->
        seq[
            divWithStyle
                "לא היה הבחרת קביצה"
                (prop.style[style.color.indianRed ; style.fontWeight.bold])
        ]
        |> ( NewAdd.Types.NewAddInfoMsg >> dispatch )

let changeFileStatus ( model : NewAdd.Types.Model ) media newStatus =
            
    let info =
        match media with
        | NewAdd.Types.Video (file,_) ->
            (NewAdd.Types.Video(file, newStatus),file)  
        | NewAdd.Types.InstructionTxt (file,_) ->
            (NewAdd.Types.InstructionTxt(file, newStatus),file)

    let fileName = info |> fun (_,x) -> x.name
    let media = info |> fun (x,_) -> x
    console.log("before")
    console.log(media)


    match model.NewInstructionData with
    | Some data ->
        data
        |> fun x -> Seq.zip x [0..x |> Seq.length |> fun y -> y - 1]
        |> Seq.tryFind (fun (mediaComp,_) ->
            match mediaComp with
            | NewAdd.Types.Video (file,_) ->
                file.name = fileName 
            | NewAdd.Types.InstructionTxt (file,_) ->
                file.name = fileName)
        |> function
            | res when res <> None ->
                res.Value
                |> fun (_,pos) ->
                    let dataAsArr = data |> Seq.toArray
                    let lastPos = dataAsArr.Length - 1
                    let newData =
                        dataAsArr
                        |> function
                            | res when res.Length = 1 ->
                                seq[media]
                            | _ when pos = 0 ->
                                Array.append [|media|] dataAsArr.[1..lastPos]
                                |> Array.toSeq
                            | _ when pos = lastPos ->
                                Array.append dataAsArr.[0..lastPos - 1] [|media|]
                                |> Array.toSeq
                            | _ ->
                                Array.append dataAsArr.[0..pos - 1] [|media|]
                                |> Array.append dataAsArr.[pos + 1..lastPos]
                                |> Array.toSeq
                    { model with NewInstructionData = Some newData}, []
            | _ -> model, []

    | _ -> model, []
