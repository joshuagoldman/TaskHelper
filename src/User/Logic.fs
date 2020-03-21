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
open Instruction.Logic

/// <summary>When login has succeeded, a delay occurrs in order to let the user
/// read login user information.</summary>
///<c>logindelay</c>
///<remarks>logindelay</remarks>
let sleepAndLogin usrData =
    async{
        do! Async.Sleep 2000
        return(
            usrData
            |> User.Types.LoginSuceeded
        )
    }

/// <summary>Establishes a delay</summary>
///<c>logindelay</c>
///<remarks>logindelay</remarks>
let delayedMessage time ( msg : 'msg ) =
    async{
        if time < 30000 && time > 0
        then do! Async.Sleep time
        else do! Async.Sleep 0

        return(msg)
    }

let spinner =
    Html.div[
        prop.className "column"
        prop.style[
            style.marginTop 15
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
        
            
            
    | InstructionSearch.Types.Instruction (instruction,id,_) ->
        (instruction,id)
        |> Instruction.Types.NewInstruction2Show 
        |> User.Types.InstructionMsg
        |> dispatch

let WritePartOrInstruction result =
    match result with
    | InstructionSearch.Types.Part (partData, _, _, _) -> partData.Title
    | InstructionSearch.Types.Instruction (instruction, _,_) -> instruction.Title

let choosePage page =
    match page with
    | InstructionSearch.Types.Part (_, _, _, _) ->
                Global.Part
    | InstructionSearch.Types.Instruction (_,_,_) ->
                Global.Instruction

let searchInfo info (keyWord : string) =
    match info with
    | InstructionSearch.Types.Instruction (instruction, _,_) -> instruction.Title.ToLower().Contains keyWord && keyWord <> ""
    | InstructionSearch.Types.Part (partData, _, _, _) -> partData.Title.ToLower().Contains keyWord && keyWord <> ""

let loadInitData data =

    let initInstruction =
        data.Instructions |> Seq.item 0

    let initPart =
        data.Instructions
        |> Seq.item 0
        |> fun x -> x.Data
        |> Seq.item 0

    let newPartMsgChaining initPart initInstruction =
        (initPart,initInstruction) |>
        (
            Part.Types.NewPart2Show >>
            Instruction.Types.PartMsg >>
            User.Types.InstructionMsg
        )

    let newInstr2ShowFuncChaining initInstruction id =
        (initInstruction, id |> string ) |>
        (
            Instruction.Types.NewInstruction2Show >>
            User.Types.InstructionMsg
        )

    let initNewAddInstruction id =
        (None,id |> string) |>
        (
            Some >>
            NewAdd.Types.NewCurrentInstructionMsg >>
            User.Types.NewAddMsg
        )
        
    seq
        [
            data.Id |> newInstr2ShowFuncChaining initInstruction
            initInstruction |> newPartMsgChaining initPart
            data.Id |> initNewAddInstruction
        ]

                       

/// <summary>Loading data from database.</summary>
///<c>database,get data</c>
let loadData ( status : Data.Deferred<Result<UserData,string>> ) =
    match status with
    | HasNostStartedYet -> "No data has been loaded to user" |> Error
        
    | InProgress -> "Data is still loading..." |> Error
        
    | Resolved response ->
        match response with
        | Ok result -> Ok result                
        | Error err -> err |> Error

//let getUserData result ( model : User.Types.Model ) =
//    result
//    |> Seq.tryFind (fun user -> user.Id = model.Id  )
//    |> function
//       | res when res <> None -> res.Value
//       | _ ->
//            {
//                Id = 0
//                Instructions =
//                    seq
//                        [
//                            {
//                                Title = ""
//                                Data =
//                                    seq
//                                        [
//                                            {
//                                                InstructionTxt = ""
//                                                InstructionVideo = ""
//                                                Title = ""
//                                            }
//                                        ]
//                            }
//                        ]

//            }

let jsonDecodingInstructions jsonString =
    let decodingObj = parseUserData jsonString

    match decodingObj with
    | Ok result ->
        result
        |> Array.item 0
        |> fun usrdata ->
            LoadedInstructions (Finished (Ok (usrdata)))
    | Error result ->
        LoadedInstructions (Finished (Error result))

let jsonDecodingUser jsonString =
    let decodingObj = LoginInfoArrayDecoder jsonString

    match decodingObj with
    | Ok result ->
        result
        |> Array.item 0
        |> (Ok >> Finished >> LoadedUsers)
    | Error result ->
        LoadedUsers (Finished (Error ("possibly wrong username or password:\n" + result)))


let loadInstructionItems ( id : int ) = async {
        do! Async.Sleep 3000
        let! response = 
            Http.request ("http://localhost:3001/api/users/" + (id |> string))
            |> Http.method GET
            |> Http.header (Headers.contentType "application/json")
            |> Http.send
        match response.statusCode with
        | 200 ->
            return ( jsonDecodingInstructions response.responseText )
            
        | _ ->
            return LoadedInstructions (Finished (Error ("Could not get api, status code: " +
                                                        (response.statusCode |> string))))  
    }

let instructionToSql userId ( instructionId : string ) instruction =

    let sqlInstructionVars =
        seq[
            userId
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

let saveInstructionToDatabase ( status : Result<Data.InstructionData * string,string> )
                                userId =
 

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
    | Ok (instruction,instructionId) ->
       let sqlCommand = instructionToSql userId instructionId instruction
       ( insertInstructionAsync sqlCommand)
       |> Async.RunSynchronously


    | Error err ->
        seq[str err]
        |> NewAdd.Types.NewAddInfoMsg
        |> Cmd.ofMsg

let createInstructionFromFile ( medias : seq<NewAdd.Types.MediaChoiceFormData>)
                              ( instruction2Add : option<Data.InstructionData> * string ) =
    let (insOpt,id) =
        instruction2Add
        |> fun (insOpt,id) -> (insOpt,id)
    let mutable videosSequence = seq[]
    let mutable instructionSequence = seq[]

    medias
    |> Seq.iter (fun mediaContent ->
                        match mediaContent with
                        | NewAdd.Types.Video (vid,_) ->
                            videosSequence <- Seq.append videosSequence [vid]
                        | NewAdd.Types.InstructionTxt (instrctn,_) ->
                            instructionSequence <- (Seq.append instructionSequence [instrctn]))

    Seq.zip3 videosSequence instructionSequence [0..videosSequence |> Seq.length |> fun x -> x - 1]
    |> Seq.map (fun (video,txt,pos) ->
                {
                    Title = "Please_provide_Title" + (pos |> string)
                    InstructionVideo = video.name
                    InstructionTxt =  txt.name
                })
    |> fun parts ->
            {
                Title = "Please_provide_Title"
                Data = parts
            }
    |> function
        | res when insOpt.IsSome ->
            let newParts =
                insOpt.Value.Data
                |> Seq.append res.Data

            {
                Title = insOpt.Value.Title
                Data = newParts
            },id
        | res -> res,id
    |> Instruction.Types.NewInstruction2Show
    |> User.Types.InstructionMsg
    |> fun x ->
        let turnToModMode =
            style.visibility.visible
            |> Instruction.Types.ModifyInstructionMsg
            |> User.Types.InstructionMsg
        let changeToInstructionMsg =
            (UserPage.Instruction, 2000)
            |> User.Types.Delay
            |> (User.Types.ChangePage)
        let msgs =
            seq[
                x
                turnToModMode
                changeToInstructionMsg
            ]
        msgs

let getNewMediaFormData oldMedia newFileInfo =
    match oldMedia with
    | NewAdd.Types.Video (vid,_) ->
        (vid,newFileInfo)
        |> NewAdd.Types.Video
    | NewAdd.Types.InstructionTxt (instrctn,_) ->
        (instrctn,newFileInfo)
        |> NewAdd.Types.InstructionTxt

let funcChaining media status =
    status |>
    (
        getNewMediaFormData media >>
        NewAdd.Types.ChangeFileStatus >>
        User.Types.NewAddMsg
    )

let saveAsync ( media : NewAdd.Types.MediaChoiceFormData )
              ( usrId )
              ( instructionName ) = async{

    let fileInfo =
        match media with
        | NewAdd.Types.Video (vid,_) ->
            vid
        | NewAdd.Types.InstructionTxt (instrctn,_) ->
            instrctn

    let fData =
        FormData.Create()

    fData.append("fname", fileInfo.name)

    let! response =
        Http.request ("http://localhost:8080/" + usrId + "/" + instructionName)
        |> Http.method POST
        |> Http.content (BodyContent.Form fData)
        |> Http.header (Headers.contentType fileInfo.``type``)
        |> Http.send

    console.log("post ended")

    match response.statusCode with
    | 200 ->
        return (
            divWithStyle
                None
                ("file was succesfully saved")
                ( prop.style[ style.color.green ; style.fontWeight.bold ] )
            |> NewAdd.Types.IsUploading.YesSuceeded
            |> funcChaining media
            
        )
    | _ ->
        return (
                divWithStyle
                    None
                    ("file failed with status code: " +
                     ( response.statusCode |> string ) + response.responseText)
                     ( prop.style[ style.color.red ; style.fontWeight.bold ] )
                |> NewAdd.Types.IsUploading.Yes
                |> funcChaining media
        )
}


let saveUserData
        ( status : SaveDataProgress<NewAdd.Types.MediaChoiceFormData * string option * string option,
                                        option<seq<NewAdd.Types.MediaChoiceFormData>>> ) =
    match status with 
    | SavingHasNostStartedYet (media,None,None) ->
        divWithStyle
            None
            "File is uploading"
            (prop.style[style.color.black ; style.fontWeight.bold] )
        |> NewAdd.Types.IsUploading.Yes
        |> funcChaining media
        |> fun x ->
            seq[
                x
                (media,None,None) |> ( SavingInProgress >>
                                 SavingOnGoing >>
                                 NewAdd.Types.CreateNewDataMsg >>
                                 User.Types.NewAddMsg)
            ]
            |> Seq.map ( fun msg -> msg |> Cmd.ofMsg )

    | SavingInProgress (media,id,instrName) ->
        ()
        |> function
            | _ when id.IsSome && instrName.IsSome ->
                instrName.Value
                |> saveAsync media id.Value
                |> Cmd.fromAsync
                |> fun x -> seq[x]
            | _ -> seq[Cmd.none]

    | SavingResolved mediasOpt ->
        match mediasOpt with
        | Some medias ->
            let isUploadFinished =
                medias
                |> Seq.map (fun media ->
                        match media with
                        | NewAdd.Types.Video (_,uploadStatus) ->
                            uploadStatus
                        | NewAdd.Types.InstructionTxt (_,uploadStatus) ->
                            uploadStatus
                    )
                |> Seq.forall (fun uploadStatus ->
                    match uploadStatus with
                    | NewAdd.Types.IsUploading.Yes _ -> false
                    | NewAdd.Types.IsUploading.No _ -> false
                    | NewAdd.Types.IsUploading.YesSuceeded _ -> true)
            ()
            |> function
                | _ when isUploadFinished = true ->
                    console.log("all were success")
                    medias |>
                    ( NewAdd.Types.PostInstruction >> User.Types.NewAddMsg)
                    |> Cmd.ofMsg
                    |> fun x -> seq[x]
                | _ ->
                    console.log("not all were success")
                    seq[Cmd.Empty]

                    
        | None -> seq[Cmd.Empty]


let loadUserItems user password = async {
    do! Async.Sleep 3000
    let! response = 
        Http.request ("http://localhost:3001/api/users/" + user + "/" + password)
        |> Http.method GET
        |> Http.header (Headers.contentType "application/json")
        |> Http.send

    let msg =
        ()
        |> function
           | _ when response.statusCode = 200 ->
                ()
                |>function
                    | _ when response.responseText = "[]" ->
                        LoadedUsers (Finished (Error ("Wrong user name or password ")))
                    | _ -> 
                        jsonDecodingUser (response.responseText)
        
           | _ ->
                LoadedUsers (Finished (Error ("Could not get api, status code: " +
                                                            (response.statusCode |> string))))
    return msg
}


let getLoginInfo model =
    let getValues obj =
        match obj with
        | Validity.Valid str -> Some str
        | Validity.Invalid -> None
    (getValues model.UserFromLogin.Username, getValues model.UserFromLogin.Password)

let getUserValidationMsg ( username : string Option )
                         ( password : string Option ) =
    ()
    |> function
        | _ when username.IsNone ->
            LoadedUsers (Finished (Error ("Invalid user name")))
            |> Cmd.ofMsg
        | _ when password.IsNone ->
            LoadedUsers (Finished (Error ("Invalid password")))
            |> Cmd.ofMsg
        
        | _ ->
             loadUserItems username.Value password.Value
             |> Cmd.fromAsync

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

let loginAttempt ( model : User.Types.Model ) ( status : Data.Deferred<Result<LoginInfo, string>> ) =
    match status with
    | HasNostStartedYet ->
        seq[
            style.visibility.visible |> User.Types.LoginSpinnerMsg
            User.Types.LoadedUsers Started
        ]
        
        
    | InProgress ->
        seq["loading user" |> User.Types.LoginMessages]
        
    | Resolved response ->
        let spinnerMessage =
            style.visibility.hidden |> LoginSpinnerMsg

        match response with
        | Ok loginInfo ->
            seq[
                    loginInfo.Id |> NewUserId
                    loginInfo.Id |> (Part.Types.NewUserIdMsg >>
                                     Instruction.Types.PartMsg >>
                                     User.Types.InstructionMsg)
                    User.Types.LoadedInstructions Started
            ]       
        | Error err -> seq[err|> User.Types.LoginMessages ; spinnerMessage]
        

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
    | _ -> seq[
                divWithStyle
                    None
                    ""
                    (prop.style[style.color.black ; style.fontWeight.bold])
           ]
        

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
                        divWithStyle
                            None
                            vid.name
                            (prop.style[style.color.black ; style.fontWeight.bold])
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
                        divWithStyle
                            (Some "columns is-centered")
                            instrctn.name
                            (prop.style[style.color.black ; style.fontWeight.bold])
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
                        None
                        "The following files did not have the the right file type:"
                        (prop.style[
                                    style.color.black
                                    style.fontWeight.bold
                                    style.fontSize 13
                        ] )
                ]
            let secondMessage = 
                res
                |> Seq.map (fun media ->
                    match media with
                    | NewAdd.Types.Video (vid,_) ->
                        seq[
                            divWithStyle
                                None
                                vid.name
                                (prop.style[
                                    style.color.indianRed
                                    style.fontWeight.bold
                                    style.fontSize 13
                                ])
                        ] 

                    | NewAdd.Types.InstructionTxt (instrctn,_) ->
                        seq[
                            divWithStyle
                                None
                                instrctn.name
                                (prop.style[
                                    style.color.indianRed
                                    style.fontWeight.bold
                                    style.fontSize 13
                                ])
                        ])
                |> Seq.collect (fun components -> components)
            let finalMessage =
                seq[
                    divWithStyle
                        None
                        "Allowed video formats are:"
                        (prop.style[
                            style.color.black
                            style.fontWeight.bold
                            style.fontSize 13
                        ])
                    divWithStyle
                        None
                        ".mpeg, .mp4, .ogg, .avi."
                        (prop.style[
                            style.color.indianRed
                            style.fontWeight.bold
                            style.fontSize 13
                        ])
                    divWithStyle
                        None
                        "For instruction text files:"
                        (prop.style[
                            style.color.black
                            style.fontWeight.bold
                            style.fontSize 13
                        ])
                    divWithStyle
                        None
                        ".md (Markdown)"
                        (prop.style[
                            style.color.indianRed
                            style.fontWeight.bold
                            style.fontSize 13
                        ])
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
                    None
                    "צריך לבחור אותו כמות של קבצי וידאו ומארקדבן"
                    (prop.style[
                        style.color.indianRed
                        style.fontWeight.bold
                        style.fontSize 13
                    ])
            ]
            |> Some

let provideNewAddPopUpWait ( ev : Types.Event )
                             wait
                             msgs =
    let positions =
        {
            X = ( ev?pageX : float )
            Y = ( ev?pageY : float )
        }
    let newPage =
        ( Global.UserPage.Instruction,wait)
        |> Delay

    let funcChaining newPage msgs =
        (msgs,newPage,positions) |>
        (
            User.Types.PopUpSettings.DefaultNewPage >>
            Some >>
            User.Types.PopUpMsg 
        )
    msgs
    |> funcChaining newPage

let provideNewAddPopUp ( ev : Types.Event )
                         dispatch
                         msgs =
    let positions =
        {
            X = ( ev?pageX : float )
            Y = ( ev?pageY : float )
        }
    let funcChaining dispatch msgs =
        (msgs,dispatch,positions) |>
        (
            User.Types.PopUpSettings.DefaultWithButton >>
            Some >>
            User.Types.PopUpMsg 
        )
    msgs
    |> funcChaining dispatch

let decideIfUploadValid ( medias : seq<NewAdd.Types.MediaChoiceFormData>)
                        ( ev : Types.MouseEvent )
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
                    None
                    "Shortly you'll be directed to modify the instruction"
                    (prop.style[
                        style.color.black
                        style.fontWeight.bold
                        style.fontSize 15
                    ])
            ]
            |> provideNewAddPopUpWait ev 2000
            |> fun x ->
                        match model.CurrentInstruction with
                        | Some instrOptWId ->
                            let instructionCreationMsg =
                                instrOptWId
                                |> createInstructionFromFile medias
                            seq[x]
                            |> Seq.append instructionCreationMsg
                            |> Seq.iter (fun msg -> (msg |> dispatch))
                        | _ -> ()
                        
                        
        | res ->
            res
            |> Seq.collect (fun msgs -> msgs.Value)
            |> provideNewAddPopUp ev dispatch
            |> dispatch

let isUploadable ( model : NewAdd.Types.Model )
                 ( ev : Types.MouseEvent )
                   dispatch =
    match model.NewInstructionData with
    | Some res ->
        res
        |> function
            | _ when res |> Seq.length = 0 ->
                seq[
                    divWithStyle
                        None
                        "לא היה הבחרת קביצה, ומספר זהות לא קיימת"
                        (prop.style[
                            style.color.indianRed
                            style.fontWeight.bold
                            style.fontSize 13
                        ])
                ]
                |> fun x ->
                    x
                    |> provideNewAddPopUp ev dispatch
                    |> dispatch
            | _ when res |> Seq.length = 0 ->
                seq[
                    divWithStyle
                        None
                        ("לא היה הבחרת קביצה")
                        (prop.style[
                            style.color.indianRed
                            style.fontWeight.bold
                            style.fontSize 13
                        ])
                ]
                |> fun x ->
                    x
                    |> provideNewAddPopUp ev dispatch
                    |> dispatch
            | _  ->
                decideIfUploadValid res ev model dispatch
    | None ->
        seq[
            divWithStyle
                None
                "לא היה הבחרת קביצה"
                (prop.style[
                    style.color.indianRed
                    style.fontWeight.bold
                    style.fontSize 13
                ])
        ]
        |> fun x ->
            x
            |> provideNewAddPopUp ev dispatch
            |> dispatch

let changeFileStatus ( model : NewAdd.Types.Model ) media =

    let (fileName,newStatus) =
        match media with
        | NewAdd.Types.Video (vid,status) ->
            vid.name,status
        | NewAdd.Types.InstructionTxt (instrctn,status) ->
            instrctn.name,status
    match model.NewInstructionData with
    | Some data ->
        data
        |> Seq.map (fun currMedia ->
            match currMedia with
            | NewAdd.Types.Video (file,_) ->
                if file.name = fileName
                then (file,newStatus) |> NewAdd.Types.Video
                else currMedia
            | NewAdd.Types.InstructionTxt (file,_) ->
                if file.name = fileName
                then (file,newStatus) |> NewAdd.Types.Video
                else currMedia)
        |> fun x ->
            { model with NewInstructionData = Some x }, []

    | _ -> model,[]

let fileHandle (ev : Types.Event)
               ( infoOpt : option<Option<Data.InstructionData> * string>  )
                 name
                 dispatch =
    let files = (ev.target?files : Types.FileList)


    infoOpt
    |> function
        | _ when infoOpt.IsSome ->
            seq[0..files.length - 1]
            |> Seq.map (fun pos -> chooseMediaByName name
                                                     files.[pos])
            |> fun medias -> (medias,name)
            |> ( NewAdd.Types.NewFilesChosenMsg >>
                    User.Types.NewAddMsg >> dispatch )
        | _ -> ()

let getPopupWindow ( popupSettings : PopUpSettings<Msg -> unit> ) =
    let defaultStyle positions =
        prop.style[
            style.zIndex 1
            Feliz.style.left ( positions.X |> int )
            Feliz.style.top ( positions.Y |> int )
            style.position.absolute
            style.backgroundColor.white
            style.borderRadius 20
            style.opacity 0.90
        ]
    match popupSettings with
    | DefaultWithButton (str,dispatch,positions) ->
        
        let style = defaultStyle positions

        let button =
            Html.div[
                prop.className "columns is-centered"
                prop.children[
                    Html.a[
                        prop.className "button"
                        prop.style[
                            Feliz.style.margin 30
                            Feliz.style.backgroundColor "grey"
                            Feliz.style.fontSize 18
                            Feliz.style.borderRadius 10
                        ]
                        prop.onClick (fun _ -> None |> ( PopUpMsg >> dispatch ) )
                        prop.children[
                            Fable.React.Helpers.str "Ok"
                        ]
                    ]
                ]
            ]

        let popupNoMsgs =
            (
                {
                    Style = style
                    Button = Some button
                    Messages = str
                },
                Cmd.none
            )
            |> Some

        popupNoMsgs

    | OptionalWithMsg (divs,positions,styles) ->
        let style =
            [
                style.zIndex 1
                Feliz.style.left ( positions.X |> int )
                Feliz.style.top ( positions.Y |> int )
                style.position.absolute
                style.backgroundColor.white
                style.borderRadius 10
                style.opacity 0.85
            ]
            |> List.append (styles |> Seq.toList)
            |> prop.style


        let popupNoMsg =
            (
                {
                    Style = style
                    Button = None
                    Messages = divs
                },
                Cmd.none
            )
            |> Some

        popupNoMsg

    | DefaultNewPage (divs,newPage,positions) ->
        let style = defaultStyle positions
        let popupNoMsg =
            {
                Style = style
                Button = None
                Messages = divs
            }

        let delayedPopupKill =
            None |>
            (
                User.Types.PopUpMsg >>
                delayedMessage 2000 >>
                Cmd.fromAsync
            )

        let msgs =
            seq[
                ( newPage |> (User.Types.ChangePage >> Cmd.ofMsg) )
                delayedPopupKill
                
            ]
            |> Cmd.batch

        (popupNoMsg,msgs) |> Some
