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
open NewAdd.Types

let getPositions ev =
    let positions =
        {
            X = ( ev?pageX : float )
            Y = ( ev?pageY : float )
        }
    positions
    

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

let errorPopupMsg positions str =
    let popUpDiv =
        Global.divWithStyle
            None
            str
            (prop.style[style.color.black;style.fontWeight.bold])
        |> fun x -> seq[x]
    (popUpDiv,positions) |>
    (
        User.Types.DefaultWithButton >>
        Some >>
        User.Types.PopUpMsg
    )

let spinner =
    Html.div[
        prop.className "column is-2"
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

let funcChainingIsUploading positions status =
    (status,positions) |>
    ( 
        Instruction.Types.ChangeFileStatus >>
        User.Types.InstructionMsg
    )

let deleteAsync ( fileName : string )
                  positions = async{

    let fData =
        FormData.Create()

    fData.append("filePath", fileName)

    do! Async.Sleep 3000
    let! response =
        Http.request ("http://localhost:3001/delete" )
        |> Http.method POST
        |> Http.content (BodyContent.Form fData)
        |> Http.send

    match response.statusCode with
    | 200 ->
        let newStatusMsg =
            (
                Html.div[
                    prop.className "column is-11"
                    prop.style[
                        style.color.red
                        style.fontWeight.bold
                        style.fontSize 12
                        style.maxWidth 400
                        style.margin 5
                       ]
                    prop.children[
                        str response.responseText
                    ]
                ]
            )
        return (
                (fileName, positions,newStatusMsg)
                |> Instruction.Types.DeleteProcess.DeleteFinished
                |> Instruction.Types.DeletePartFilesMsg
                |> User.Types.InstructionMsg
        )
    | _ ->
        let msg =
            ("file \"" + fileName + "\" failed with status code: " +
             ( response.statusCode |> string ) + response.responseText)
        let newStatusMsg =
            (
                Html.div[
                    prop.className "column is-11"
                    prop.style[
                        style.color.red
                        style.fontWeight.bold
                        style.fontSize 12
                        style.maxWidth 400
                        style.margin 5
                       ]
                    prop.children[
                        str msg
                    ]
                ]
            )
        return (
                (fileName, positions,newStatusMsg)
                |> Instruction.Types.DeleteProcess.DeleteFinished
                |> Instruction.Types.DeletePartFilesMsg
                |> User.Types.InstructionMsg
        )
}

let instructionToSqlSaveNew userId
                            ( instructionId : string )
                            ( databaseNewFilesOptions : DatabaseNewFilesOptions ) =

    let fullPath name =
        String.Format(
            "User_{0}/Instruction_{1}/{2}",
            userId,
            instructionId,
            name
        )

    let getPartInsert parts =
        parts
        |> Seq.map (fun part ->
              String.Format(
                "INSERT INTO parts ( id,instruction_id, instruction_video, instruction_txt, part_title)
                VALUES ( {0}, {1}, '{2}', '{3}','{4}');",
                userId,
                instructionId,
                fullPath part.InstructionVideo,
                fullPath part.InstructionTxt,
                part.Title
              ))
        |> String.concat ""

    match databaseNewFilesOptions with
    | DatabaseNewFilesOptions.NewInstructionOption instruction ->
        let instructionInsert =
            String.Format(
                "INSERT INTO instructions ( id, instruction_id, title )
                 VALUES ( {0}, {1}, '{2}');",
                 userId,
                 instructionId,
                 instruction.Title)

        let partInsert =
            getPartInsert instruction.Data

        instructionInsert + partInsert

    | DatabaseNewFilesOptions.SameInstructionOption instruction ->
        let partInsert =
            getPartInsert instruction.Data

        partInsert
    
let instructionToSqlNewNames userId ( instructionId : string ) instruction =

    let instructionInsert =
        String.Format(
            "UPDATE instructions SET title = '{0}' WHERE id = {1} AND instruction_id = {2}",
            instruction.Title,
            userId,
            instructionId
        )

    let partInsert =
        instruction.Data
        |> Seq.map (fun part ->
              String.Format(
                "UPDATE parts SET part_title = {0] WHERE instruction_id = {1} AND instruction_video = {2} AND instruction_txt = {3};",
                part.Title,
                instructionId,
                part.InstructionVideo,
                part.InstructionTxt
              ))
        |> String.concat ""

    instructionInsert + partInsert

let instructionToSqlDelete ( dbOptions : DatabaseDeleteOptions ) ids =

    let partDelete usrId instructionId parts =
        parts
        |> Seq.map (fun part ->
              String.Format(
                "DELETE FROM parts WHERE id = {0} AND instruction_id = {1} AND instruction_video = '{2}' AND instruction_txt = '{3}' AND part_title = '{4}';",
                usrId,
                instructionId,
                part.InstructionVideo,
                part.InstructionTxt,
                part.Title
              ))
        |> String.concat ""
    match dbOptions with
    | DatabaseDeleteOptions.DeleteInstruction instruction ->
        let instructionDelete =
            String.Format(
                "DELETE FROM instructions WHERE id = {0} AND instruction_id = {1} AND title = '{2}';",
                ids.UserId,
                ids.InstructionId,
                instruction.Title)
        let partsCommand =
            instruction.Data
            |> partDelete ids.UserId ids.InstructionId

        instructionDelete + partsCommand
    | DeleteParts instr ->
        let partsCommand =
            instr.Data
            |> partDelete ids.UserId ids.InstructionId
        partsCommand

let sqlCommandToDB databaseOptions ids positions = async{
    let sqlCommands =
        databaseOptions
        |> Seq.map (fun option ->
            match option with
            | DatabaseSavingOptions.NewFilesInstruction savingOptions ->
                savingOptions
                |> instructionToSqlSaveNew ids.UserId ids.InstructionId
            | DatabaseSavingOptions.NewNameInstruction instr ->
                instr
                |> instructionToSqlNewNames ids.UserId ids.InstructionId
            | DatabaseSavingOptions.PartsToDeleteInstruction delOption ->
                ids
                |> instructionToSqlDelete delOption)
        |> String.concat ""

    let fd = FormData.Create()
    fd.append ("Queries", sqlCommands)

    do! Async.Sleep 3000
    let! response =
        Http.request ("http://localhost:3001/" )
        |> Http.method POST
        |> Http.content (BodyContent.Form fd)
        |> Http.send

    let funcChaining successOrNot =
        successOrNot |>
        (
            Instruction.Types.DatabaseChangeProcess.DatabseChangeFinished >>
            Instruction.Types.Msg.DatabaseChangeMsg >>
            User.Types.InstructionMsg
        )

    let responseDiv msg =
        Html.div[
            prop.className "column is-1"
            prop.style[
                style.color.red
                style.fontWeight.bold
                style.fontSize 12
                style.maxWidth 400
                   ] 
            prop.children[
                str response.responseText
            ]
        ]

    match response.statusCode with
    | 200 ->
        let newStatus =
            (
               responseDiv response.responseText ,
               positions,
               databaseOptions
            )
            |> Instruction.Types.DatabaseChangeSucceeded
            |> funcChaining 
            
        return newStatus
    | _ ->
        let newStatus =
            (
                responseDiv response.responseText,
                positions
            )
            |> Instruction.Types.DatabaseChangeFailed
            |> funcChaining 
            
        return newStatus
}

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
                        | NewAdd.Types.Video vid ->
                            videosSequence <- Seq.append videosSequence [vid]
                        | NewAdd.Types.InstructionTxt instrctn ->
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
            |> fun msg1 ->
                seq[
                    msg1
                    (false |>
                     Instruction.Types.DeleteButtonEnablenMsg)
                ]
            |> Seq.map (fun msg -> msg |> User.Types.InstructionMsg)
        let changeToInstructionMsg =
            (UserPage.Instruction, 2000)
            |> User.Types.Delay
            |> (User.Types.ChangePage)
        let msgs =
            turnToModMode
            |> Seq.append(
                    seq[
                        x
                        changeToInstructionMsg
                    ]
                )
        msgs

let saveAsync ( file : Types.File )
              ( options : DatabaseNewFilesOptions )
                positions
               ( ids : DBIds ) = async{

    let fullPath =
        String.Format(
            "User_{0}/Instruction_{1}/{2}",
            ids.UserId,
            ids.InstructionId,
            file.name
        )

    let fData =
        FormData.Create()

    let checkSavingMsg =
        (ids,positions,options)
        |> Instruction.Types.CheckIfSaveFinished
        |> User.Types.InstructionMsg

    fData.append("filePath", fullPath)
    fData.append("file", file)


    do! Async.Sleep 3000
    let! response =
        Http.request ("http://localhost:3001/upload" )
        |> Http.method POST
        |> Http.content (BodyContent.Form fData)
        |> Http.send

    match response.statusCode with
    | 200 ->
        let newStatus =
            (
                file.name,
                Html.div[
                    prop.className "column is-11"
                    prop.style[
                        style.color.red
                        style.fontWeight.bold
                        style.fontSize 12
                        style.maxWidth 400
                        style.margin 5
                       ]
                    prop.children[
                        str response.responseText
                    ]
                ]
            )
            |> Instruction.Types.UploadOrDeleteFinishedSuccesfully
        return (
                newStatus
                |> funcChainingIsUploading positions
                |> fun x ->
                    seq[
                        x
                        checkSavingMsg
                    ]
                    |> Seq.map (fun msg -> msg |> Cmd.ofMsg)
                    |> Cmd.batch
                    |> User.Types.CmdMsging
        )
    | _ ->
        let msg =
            ("file \"" + file.name + "\" failed with status code: " +
             ( response.statusCode |> string ) + response.responseText)
        let newStatus =
            (
                file.name,
                Html.div[
                    prop.className "column is-11"
                    prop.style[
                        style.color.red
                        style.fontWeight.bold
                        style.fontSize 12
                        style.maxWidth 400
                        style.margin 5
                       ]
                    prop.children[
                        str msg
                    ]
                ]
            )
            |> Instruction.Types.PartStatus.UploadOrDeleteFinishedWithFailure
        return (
                newStatus
                |> funcChainingIsUploading positions
                |> fun x ->
                    seq[
                        x
                        checkSavingMsg
                    ]
                    |> Seq.map (fun msg -> msg |> Cmd.ofMsg)
                    |> Cmd.batch
                    |> User.Types.CmdMsging
        )
}

let saveUserData
        ( status : SaveDataProgress<(Types.File * DBIds * Position * DatabaseNewFilesOptions),
                                        seq<DatabaseSavingOptions> * DBIds * Position>) =
    match status with 
    | SavingHasNostStartedYet(file,dbIds,positions,options) ->
        file.name
        |> Instruction.Types.Uploading
        |> funcChainingIsUploading positions
        |> fun x ->
            let funcChainingSavingInProgress info =
                info |>
                (
                    SavingInProgress >>
                    NewAdd.Types.CreateNewDataMsg >>
                    User.Types.NewAddMsg
                )
            seq[
                x
                (file,dbIds,positions,options)
                |> funcChainingSavingInProgress
            ]
            |> Seq.map ( fun msg -> msg |> Cmd.ofMsg )

    | SavingInProgress(file,dbIds,positions,options) ->
        let savingMsg =  
            dbIds
            |> saveAsync file options positions
            |> Cmd.fromAsync

        seq[savingMsg]

    | SavingResolved(savingOptions,ids,positions) ->
        let dbMsg =
            (savingOptions,ids,positions)
            |> Instruction.Types.DatabaseChangeBegun
            |> Instruction.Types.Msg.DatabaseChangeMsg
            |> User.Types.InstructionMsg
            |> Cmd.ofMsg
            |> fun x -> seq[x]

        dbMsg

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
            NewAdd.Types.Video file
        | res when res = "instructions" ->
            NewAdd.Types.InstructionTxt file
        | _ -> NewAdd.Types.Video file     

let extractFileNames files name =
        
    files
    |> Seq.map (fun media ->
        name
        |> function
            | res when res = "videos" ->
                match media with
                | NewAdd.Types.Video vid ->
                    Some vid
                | NewAdd.Types.InstructionTxt _ -> None
            | _ ->
                match media with
                | NewAdd.Types.Video _ -> None
                | NewAdd.Types.InstructionTxt instr ->
                    Some instr)
    |> Seq.choose id
    |> Seq.map (fun file -> divWithFileName file)
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
                    | NewAdd.Types.Video _ -> true
                    | NewAdd.Types.InstructionTxt _ -> false
                | _ ->
                    match media with
                    | NewAdd.Types.Video _ -> false
                    | NewAdd.Types.InstructionTxt _ -> true)
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
                   name =
    match medias with
    | Some vids->
        newVids
        |> Seq.append (removeOldOfSame vids name)
    | None -> newVids

let decideIfRightFormat ( medias : seq<NewAdd.Types.MediaChoiceFormData>) =
    medias
    |> Seq.filter (fun media ->
        match media with
        | NewAdd.Types.Video vid ->
            vid.``type`` <> "video/mpeg" &&
            vid.``type`` <> "video/ogg" &&
            vid.``type`` <> "video/mp4" &&
            vid.``type`` <> "video/avi"

        | NewAdd.Types.InstructionTxt instrctn ->
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
                    | NewAdd.Types.Video vid ->
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

                    | NewAdd.Types.InstructionTxt instrctn ->
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
        | NewAdd.Types.Video vid ->
            videos <- videos |> Seq.append [vid]
        | NewAdd.Types.InstructionTxt instrctn ->
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
                         msgs =
    let positions =
        {
            X = ( ev?pageX : float )
            Y = ( ev?pageY : float )
        }
    let funcChaining msgs =
        (msgs,positions) |>
        (
            User.Types.PopUpSettings.DefaultWithButton >>
            Some >>
            User.Types.PopUpMsg 
        )
    msgs
    |> funcChaining

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
            |> provideNewAddPopUp ev
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
                    |> provideNewAddPopUp ev
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
                    |> provideNewAddPopUp ev
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
            |> provideNewAddPopUp ev
            |> dispatch

let fileHandle ( ev : Types.Event)
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

let getPopupWindow ( popupSettings : PopUpSettings<User.Types.Msg> ) =
    let defaultStyle positions =
        prop.style[
            style.zIndex 1
            Feliz.style.left ( positions.X |> int )
            Feliz.style.top ( positions.Y |> int )
            style.position.absolute
            style.backgroundColor.white
            style.fontSize 13
            style.borderRadius 20
            style.opacity 0.90
        ]
    match popupSettings with
    | PopUpSettings.DefaultWithOptions (divs,positions,msgs) ->
        let buttonSettings =
            seq[
                Feliz.style.margin 30
                Feliz.style.backgroundColor "grey"
                Feliz.style.fontSize 18
                Feliz.style.borderRadius 10
            ]

        let popupNoMsg =
            (
                {
                    Style = positions |> defaultStyle
                    ButtonSettings = buttonSettings |> Some
                    ClickMessages = msgs |> Some
                    Messages = divs
                },
                Cmd.none
            )
            |> Some

        popupNoMsg
    | DefaultWithButton (str,positions) ->
        
        let style = defaultStyle positions

        let buttonSettings =
            seq[
                Feliz.style.margin 30
                Feliz.style.backgroundColor "grey"
                Feliz.style.fontSize 18
                Feliz.style.borderRadius 10
            ]


        let popupNoMsgs =
            (
                {
                    Style = style
                    ButtonSettings = Some buttonSettings
                    Messages = str
                    ClickMessages = None
                },
                Cmd.none
            )
            |> Some

        popupNoMsgs

    | Default (str,positions) ->
        
        let style = defaultStyle positions

        let popupNoMsgs =
            (
                {
                    Style = style
                    ButtonSettings = None
                    Messages = str
                    ClickMessages = None
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
                    ButtonSettings = None
                    Messages = divs
                    ClickMessages = None
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
                ButtonSettings = None
                Messages = divs
                ClickMessages = None
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

let savingChoicesTestable   instruction
                          ( instructionInfo : seq<Instruction.Types.modificationInfo> Option )
                            userDataInstructions =

    let compareWithExistingInstruction newInstruction =
        userDataInstructions
        |> Seq.indexed
        |> Seq.tryFind (fun (_,existInstr) ->
            existInstr.Title.Replace(" ", "") = newInstruction.Title.Replace(" ", ""))
        |> function
            | res when res.IsSome ->
                let (pos,existingInstr) =
                    res.Value
                    |> fun (x,y) -> (x,y)
                let instrId =
                    pos |> string

                let allTitlesUnique =
                    newInstruction.Data
                    |> Seq.map (fun newPart ->
                        newInstruction.Data
                        |> Seq.filter (fun newPartCompare ->
                                let result =
                                    newPart.Title.Replace(" ", "") =
                                        newPartCompare.Title.Replace(" ", "")
                                result)
                        |> function
                            | res when res |> Seq.length = 1 ->
                                None
                            | _ -> Some newPart.Title 
                            )
                    |> Seq.choose id
                    |> function
                        | res when res |> Seq.length <> 0 ->
                            res
                            |> Seq.distinct
                            |> Seq.map (fun title ->
                                "\"" + title + "\", ")
                            |> String.concat ""
                            |> fun str -> str.Substring(0,str.LastIndexOf(", "))
                            |> Some
                        | _ -> None

                allTitlesUnique
                |> function
                    | res when res.IsSome ->
                        let errorMsg =
                            String.Format(
                               "Instruction part title/titles: {0} are not unique for the instruction.
Kindly re-name instruction part/parts such that all are of distinct nature.",
                                 res.Value
                            )
                        let finalMsg = 
                            errorMsg
                            |> User.Types.newSaveResult.InstructionHasNotDistinctTitles
                        finalMsg
                    | _ ->
                        let alreadyExistingInstruction =
                            let newAndOldInstructionAreOfSameLength =
                                existingInstr.Data |> Seq.length =
                                    (newInstruction.Data |> Seq.length)

                            let newAndOldInstructionHaveSameParts =
                                newInstruction.Data
                                |> Seq.forall (fun newPart ->
                                    existingInstr.Data
                                    |> Seq.exists (fun part ->
                                        let sameTitle =
                                            newPart.Title.Replace(" ", "") =
                                                part.Title.Replace(" ", "")
                                        let sameInstructionText =
                                            newPart.InstructionTxt.Replace(" ", "") =
                                                part.InstructionTxt.Replace(" ", "")
                                        let sameInstructionVideo =
                                            newPart.InstructionVideo.Replace(" ", "") =
                                                part.InstructionVideo.Replace(" ", "")
                                        sameTitle &&
                                        sameInstructionText &&
                                        sameInstructionVideo))
                            newAndOldInstructionAreOfSameLength &&
                            newAndOldInstructionHaveSameParts
                        ()
                        |> function
                            | _ when alreadyExistingInstruction = true ->
                                "Nothing to save!"
                                |> User.Types.newSaveResult.ThatInstructionAlreadyExists
                            | _ ->
                                let newFileParts =
                                    newInstruction.Data
                                    |> Seq.choose (fun newPart ->
                                        existingInstr.Data
                                        |> Seq.forall (fun part ->
                                            let notSameInstructionText =
                                                part.InstructionTxt.Replace(" ", "") <>
                                                    newPart.InstructionTxt.Replace(" ", "")

                                            let notSameVideo =
                                                part.InstructionVideo.Replace(" ", "") <>
                                                    newPart.InstructionVideo.Replace(" ", "")
                                            notSameInstructionText || notSameVideo)
                                        |> function
                                            | noInstructionTextIsMatch when noInstructionTextIsMatch = true ->
                                                Some newPart
                                            | _ -> None)
                                    |> function
                                        | partsWNewTitles when partsWNewTitles |> Seq.length <> 0 ->
                                            partsWNewTitles |> Some
                                        | _ -> None
                                let partsWithNewNames =
                                    newInstruction.Data
                                    |> Seq.choose (fun newPart ->
                                        existingInstr.Data
                                        |> Seq.tryFind (fun part ->
                                            let nameDoesNotMatch =
                                                newPart.Title.Replace(" ", "") <>
                                                    part.Title.Replace(" ", "")

                                            let sameInstructionText =
                                                part.InstructionTxt.Replace(" ", "") =
                                                    newPart.InstructionTxt.Replace(" ", "")

                                            let sameVideo =
                                                part.InstructionVideo.Replace(" ", "") =
                                                    newPart.InstructionVideo.Replace(" ", "")

                                            nameDoesNotMatch &&
                                            sameInstructionText &&
                                            sameVideo
                                            )
                                        |> function
                                            | newTitlePart when newTitlePart.IsSome ->
                                                Some newPart
                                            | _ -> None)
                                    |> function
                                        | partsWNewTitles when partsWNewTitles |> Seq.length <> 0 ->
                                            partsWNewTitles |> Some
                                        | _ -> None

                                let partsToDelete =
                                    existingInstr.Data
                                    |> Seq.choose (fun part ->
                                        newInstruction.Data
                                        |> Seq.tryFind (fun partNew ->

                                            let sameInstructionText =
                                                part.InstructionTxt.Replace(" ", "") =
                                                    partNew.InstructionTxt.Replace(" ", "")

                                            let sameVideo =
                                                part.InstructionVideo.Replace(" ", "") =
                                                    partNew.InstructionVideo.Replace(" ", "")

                                            sameInstructionText &&
                                             sameVideo
                                            )
                                        |> function
                                            | newTitlePart when newTitlePart.IsSome ->
                                                None
                                            | _ -> Some part )
                                    |> function
                                        | partsToDelete when partsToDelete |> Seq.length <> 0 ->
                                            Some partsToDelete
                                        | _ -> None
                                ()
                                |> function
                                    | _ when partsWithNewNames.IsSome &&
                                                newFileParts.IsSome &&
                                                partsToDelete.IsSome ->
                                            let info =
                                                seq[
                                                    { newInstruction with Data = partsWithNewNames.Value }
                                                    |> DatabaseSavingOptions.NewNameInstruction

                                                    { newInstruction with Data = newFileParts.Value }
                                                    |> DatabaseNewFilesOptions.SameInstructionOption
                                                    |> DatabaseSavingOptions.NewFilesInstruction

                                                    
                                                    { newInstruction with Data = partsToDelete.Value } |>
                                                    (DatabaseDeleteOptions.DeleteParts >>
                                                     DatabaseSavingOptions.PartsToDeleteInstruction)
                                                ]

                                            (info,instrId)
                                            |> User.Types.newSaveResult.SaveExistingNewFilesAndTItlesPartsToDelete

                                    | _ when partsWithNewNames.IsSome &&
                                             newFileParts.IsSome ->
                                             let info =
                                                 seq[
                                                     { newInstruction with Data = newFileParts.Value }
                                                     |> DatabaseNewFilesOptions.SameInstructionOption
                                                     |> DatabaseSavingOptions.NewFilesInstruction

                                                     { newInstruction with Data = partsWithNewNames.Value }
                                                     |> DatabaseSavingOptions.NewNameInstruction
                                                 ]

                                             (info,instrId)
                                             |> User.Types.newSaveResult.SaveExistingNewFilesAndTItles
                                    | _ when newFileParts.IsSome &&
                                             partsToDelete.IsSome ->
                                            let info =
                                                seq[
                                                    { newInstruction with Data = newFileParts.Value }
                                                    |> DatabaseNewFilesOptions.SameInstructionOption
                                                    |> DatabaseSavingOptions.NewFilesInstruction

                                                    { newInstruction with Data = partsToDelete.Value } |>
                                                    (DatabaseDeleteOptions.DeleteParts >>
                                                     DatabaseSavingOptions.PartsToDeleteInstruction)
                                                ]

                                            (info,instrId)
                                            |> User.Types.newSaveResult.SaveExistingNewFilesPartsToDelete
                                    | _ when partsWithNewNames.IsSome &&
                                             partsToDelete.IsSome ->
                                            let info =
                                                seq[
                                                    { newInstruction with Data = partsWithNewNames.Value }
                                                    |> DatabaseSavingOptions.NewNameInstruction

                                                    { newInstruction with Data = partsToDelete.Value } |>
                                                    (DatabaseDeleteOptions.DeleteParts >>
                                                     DatabaseSavingOptions.PartsToDeleteInstruction)
                                                ]

                                            (info,instrId)
                                            |> User.Types.newSaveResult.SaveExistingNewTItlesPartsToDelete
                                    | _ when newFileParts.IsSome ->
                                        let info =
                                            seq[
                                                { newInstruction with Data = newFileParts.Value }
                                                |> DatabaseNewFilesOptions.SameInstructionOption
                                                |> DatabaseSavingOptions.NewFilesInstruction
                                            ]

                                        (info,instrId)
                                        |> User.Types.newSaveResult.SaveExisitngNewFIles
                                    | _ when partsWithNewNames.IsSome ->
                                        let info =
                                            seq[
                                                { newInstruction with Data = partsWithNewNames.Value }
                                                |> DatabaseSavingOptions.NewNameInstruction
                                            ]

                                        (info,instrId)
                                        |> User.Types.newSaveResult.SaveExistingNewTitles
                                    | _ ->
                                        let info =
                                            seq[
                                                { newInstruction with Data = partsToDelete.Value } |>
                                                (DatabaseDeleteOptions.DeleteParts >>
                                                 DatabaseSavingOptions.PartsToDeleteInstruction)
                                            ]

                                        (info,instrId)
                                        |> User.Types.newSaveResult.SaveExistingPartsToDelete
            | _ ->
                let instrid =
                    userDataInstructions
                    |> Seq.indexed
                    |> Seq.last
                    |> fun (pos,_) ->
                        pos + 1
                        |> string
                (newInstruction,instrid)
                |> User.Types.newSaveResult.SaveNew
                
    match instructionInfo with
    | Some modInfos ->
        instruction.Data
        |> Seq.map (fun part ->
            modInfos
            |> Seq.tryFind (fun modInfo ->
                modInfo.Names.CurrName.Trim() = part.Title.Trim())
            |> function
                | res when res.IsSome ->
                    match res.Value.DelOrReg with
                    | Some delInfo ->
                        match delInfo with
                        | Instruction.Types.DeleteInfo.Delete _ ->
                            Some part
                        | Instruction.Types.DeleteInfo.Regret _ ->
                            None
                    | _ -> None
                | _ -> None)
        |> Seq.choose id 
        |> function
            | res when res |> Seq.length <> 0 ->
                { instruction with Data = res }
                |> compareWithExistingInstruction
                
            | _ ->
                ("You are attempting to save an empty instruction.
                Click the delete button if you wish to delete the instruction")
                |> User.Types.newSaveResult.InstructionIsDelete
                
    | _ ->
        "No media has been loaded, re-upload your shit"
        |> User.Types.newSaveResult.NoUserData


let savingChoices userDataOpt positions instruction instructionInfo =
    match userDataOpt with
    | Resolved( Ok data) ->
        let result =
            savingChoicesTestable instruction
                                  instructionInfo
                                  data.Instructions

        let newStatus statusMsg =
            let msgDiv =
                Html.div[
                    prop.className "columns is-1"
                    prop.style[
                        style.margin 5
                    ]
                    prop.children[
                        str statusMsg
                    ]
                ]
            Html.div[
                prop.className "columns is-centered"
                prop.style[
                    style.color.red
                    style.fontWeight.bold
                    style.fontSize 12
                    style.maxWidth 400
                       ]
                prop.children[
                    msgDiv
                ]
            ]
            |> fun x -> seq[x]

        let funcChainingOptions positions popupMsg msgs =
            
            (popupMsg |> newStatus,positions,msgs) |>
            (
                PopUpSettings.DefaultWithOptions >>
                Some >>
                User.Types.PopUpMsg >>
                Cmd.ofMsg
            )

        let funcChaining positions popupMsg =
            
            (popupMsg |> newStatus,positions) |>
            (
                PopUpSettings.DefaultWithButton >>
                Some >>
                User.Types.PopUpMsg
            )

        let saveNewMsg newInstr instrId =
            let dbIds =
                {
                    UserId = data.Id |> string
                    InstructionId = instrId
                }
            (
                newInstr,
                dbIds,
                positions
            )
            |> (NewAdd.Types.SaveNewData >>
                User.Types.NewAddMsg)

        let getDatabaseMsg instrId savingOptions  =
            let dbIds =
                {
                    UserId = data.Id |> string
                    InstructionId = instrId
                }
            let loadingMsg =
                "Saving Changes..."

            let popupMsg =
                loadingMsg
                |> funcChaining positions
                |> Cmd.ofMsg

            
            let dbMsg =
                (savingOptions,dbIds,positions)
                |> Instruction.Types.DatabaseChangeBegun
                |> Instruction.Types.Msg.DatabaseChangeMsg
                |> User.Types.InstructionMsg
                |> Cmd.ofMsg


            seq[
                popupMsg
                dbMsg
            ]
            |> Cmd.batch
            |> User.Types.Msg.CmdMsging

        let createNewSaveAndDBChangeMsgs savingOptions instrId =
            let newInstr =
                savingOptions
                |> Seq.tryPick (fun opt ->
                    match opt with
                    | DatabaseSavingOptions.NewFilesInstruction newInstr ->
                        Some newInstr
                    | _ -> None)

            let onlyDbChange =
                savingOptions
                |> Seq.choose (fun opt ->
                    match opt with
                    | DatabaseSavingOptions.NewFilesInstruction _ ->
                        None
                    | _ -> Some opt)
                |> function
                    | onlyDbChange when onlyDbChange |> Seq.length <> 0 ->
                        Some onlyDbChange
                    | _ -> None

            ()
            |> function
                | _ when newInstr.IsSome && onlyDbChange.IsSome ->

                    let newSaveMsg =
                        instrId
                        |> saveNewMsg newInstr.Value
                        |> Cmd.ofMsg

                    let dbMsg =
                        onlyDbChange.Value
                        |> getDatabaseMsg instrId
                        |> Cmd.ofMsg

                    seq[
                        newSaveMsg
                        dbMsg
                    ]
                    |> Cmd.batch
                    |> User.Types.CmdMsging
                    |> fun x -> seq[x]

                | _ when newInstr.IsSome ->

                    let newSaveMsg =
                        instrId
                        |> saveNewMsg newInstr.Value

                    newSaveMsg
                    |> fun x -> seq[x]

                | _ when onlyDbChange.IsSome ->

                    let dbMsg =
                        onlyDbChange.Value
                        |> getDatabaseMsg instrId

                    dbMsg
                    |> fun x -> seq[x]

                | _ -> seq[User.Types.Msg.MsgNone]
            
        let msg =
            match result with
            | SaveNew (newInstr,instrId) ->
                let popupMsg =
                    "Are you sure you want to save a new instruction?"

                let savingOptions =
                    newInstr
                    |> DatabaseNewFilesOptions.NewInstructionOption
                    |> DatabaseSavingOptions.NewFilesInstruction
                    |> fun x -> seq[x]

                createNewSaveAndDBChangeMsgs savingOptions instrId
                |> funcChainingOptions positions popupMsg
            | SaveExistingNewTitles (savingOptions,instrId) ->
                let popupMsg =
                    "Are you sure you want to save an existing instruction with new titles?"

                createNewSaveAndDBChangeMsgs savingOptions instrId
                |> funcChainingOptions positions popupMsg
            | SaveExisitngNewFIles (savingOptions,instrId) ->
                let popupMsg =
                    "Are you sure you want to save existing instruction with new files?"

                createNewSaveAndDBChangeMsgs savingOptions instrId
                |> funcChainingOptions positions popupMsg
                
            | SaveExistingNewFilesAndTItles (savingOptions,instrId) ->
                let popupMsg =
                    "Are you sure you want to save existing instruction with new files and titles?"

                createNewSaveAndDBChangeMsgs savingOptions instrId
                |> funcChainingOptions positions popupMsg
            | SaveExistingNewFilesPartsToDelete (savingOptions,instrId) ->
                let popupMsg =
                    "Are you sure you want to save existing instruction with new files?"

                createNewSaveAndDBChangeMsgs savingOptions instrId
                |> funcChainingOptions positions popupMsg
            | SaveExistingNewTItlesPartsToDelete (savingOptions,instrId) ->
                let popupMsg =
                    "Are you sure you want to save existing instruction with new part titles?"

                createNewSaveAndDBChangeMsgs savingOptions instrId
                |> funcChainingOptions positions popupMsg
            | SaveExistingNewFilesAndTItlesPartsToDelete (savingOptions,instrId) ->
                let popupMsg =
                    "Are you sure you want to save existing instruction with new files and titles?"

                createNewSaveAndDBChangeMsgs savingOptions instrId
                |> funcChainingOptions positions popupMsg
            | SaveExistingPartsToDelete (savingOptions,instrId) ->
                let popupMsg =
                    "Are you sure you want to save the changes?"

                createNewSaveAndDBChangeMsgs savingOptions instrId
                |> funcChainingOptions positions popupMsg
            | InstructionIsDelete errorMsg ->
                errorMsg
                |> funcChaining positions
                |> Cmd.ofMsg
            | NoUserData errorMsg ->
                errorMsg
                |> funcChaining positions
                |> Cmd.ofMsg
            | ThatInstructionAlreadyExists errorMsg ->
                errorMsg
                |> funcChaining positions
                |> Cmd.ofMsg
            | InstructionHasNotDistinctTitles errorMsg ->
                errorMsg
                |> funcChaining positions
                |> Cmd.ofMsg
        msg
    |   _ ->
        []
