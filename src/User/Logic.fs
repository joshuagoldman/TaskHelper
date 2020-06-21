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
open System.Net
open TaskHelperJsInterop
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Collections

let (^&)x = (x)

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

let fullPath name ids =
     String.Format(
         "User_{0}/Instruction_{1}/{2}",
         ids.UserId,
         ids.InstructionId,
         name
     )

let errorPopupMsg positions str =
    let popUpDiv =
        Global.divWithStyle
            None
            str
            (prop.style[style.color.black;style.fontWeight.bold])
        |> fun x -> [|x|]
    (popUpDiv,positions) |>
    (
        User.Types.DefaultWithButton >>
        Some >>
        User.Types.PopUpMsg
    )

let spinner =
    Html.div[
        prop.className "column is-3"
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
    | InstructionSearch.Types.Instruction (instruction, _,_) ->
        match instruction.Title with
        | Data.InstructionTitleInfo.HasOldName title -> title
        | Data.InstructionTitleInfo.HasNewName titles -> titles.OldName

let choosePage page =
    match page with
    | InstructionSearch.Types.Part (_, _, _, _) ->
                Global.Part
    | InstructionSearch.Types.Instruction (_,_,_) ->
                Global.Instruction

let searchInfo info (keyWord : string) =
    match info with
    | InstructionSearch.Types.Instruction (instruction, _,_) ->
        match instruction.Title with
        | Data.InstructionTitleInfo.HasOldName title ->
            title.ToLower().Contains keyWord && keyWord <> ""
        | Data.InstructionTitleInfo.HasNewName titles ->
            false
    | InstructionSearch.Types.Part (partData, _, _, _) -> partData.Title.ToLower().Contains keyWord && keyWord <> ""

let loadInitData data =

    let initInstruction =
        data.Instructions |> Array.item 0

    let initPart =
        data.Instructions
        |> Array.item 0
        |> fun x -> x.Data
        |> Array.item 0

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
        
    
    [|
        data.Id |> newInstr2ShowFuncChaining initInstruction
        initInstruction |> newPartMsgChaining initPart
        data.Id |> initNewAddInstruction
    |]

                       

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
            LoadedInstructions (AsyncOperationEvent.Finished (Ok (usrdata)))
    | Error result ->
        LoadedInstructions (AsyncOperationEvent.Finished (Error result))

let jsonDecodingUser jsonString dispatch =
    let decodingObj = LoginInfoArrayDecoder jsonString

    match decodingObj with
    | Ok result ->
        result
        |> fun loginInfo ->
            (loginInfo |> (Array.item 0),dispatch)
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
            return LoadedInstructions (AsyncOperationEvent.Finished (Error ("Could not get api, status code: " +
                                                        (response.statusCode |> string))))  
    }

let funcChainingIsUploading utils status =
    (status,utils) |>
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

    let getPartInsert parts =
        parts
        |> Array.map (fun part ->
              String.Format(
                "INSERT INTO parts ( id,instruction_id, instruction_video, instruction_txt, part_title)
                VALUES ( {0}, {1}, '{2}', '{3}','{4}');",
                userId,
                instructionId,
                part.InstructionVideo,
                part.InstructionTxt,
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
        |> Array.map (fun part ->
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
        |> Array.map (fun part ->
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
        |> Array.map (fun option ->
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
                str msg
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

let changeFileNameIfNotUnique instructions medias =

    let getFileName ( name : string ) =
        name.Substring(name.LastIndexOf("/") + 1 )

    let allCurrentFileNames =
        instructions
        |> Array.collect (fun instruction ->
            instruction.Data
            |> Array.collect (fun part ->
                [|
                    part.InstructionTxt |> getFileName
                    part.InstructionVideo |> getFileName
                |]))
        |> String.concat "$"
        |> fun str -> "$" + str + "$"

    medias
    |> Array.map (fun media ->
        match media with
        | NewAdd.Types.Video vid ->
            (vid,Video vid)
        | NewAdd.Types.InstructionTxt instrtxt ->
            (instrtxt,InstructionTxt instrtxt))
    |> Array.map (fun (file,mediaType) ->
        let fileNameCutFormat =
            file.name.Substring(0,file.name.LastIndexOf(".")).Replace("/","")

        let pattern = fileNameCutFormat + ".*?(?=\$)"

        let matchingFileNames = Regex.Matches pattern allCurrentFileNames

        match matchingFileNames with
        | Some matchingNames ->
            let indx =
                matchingNames
                |> Array.indexed
                |> Array.map (fun (indx,_) -> indx)
                |> Array.last
                |> fun indx -> indx + 1 |> string

            let fileNameNew = file.name.Replace(".", "_" + indx + ".")

            (mediaType,fileNameNew)

        | _ -> (mediaType,file.name))

let createInstructionFromFile ( medias : (MediaChoiceFormData * string)[])
                              ( instruction2Add : InstructionData option )
                              ( ids : DBIds ) =

    let fullPath name =
        String.Format(
            "User_{0}/Instruction_{1}/{2}",
            ids.UserId,
            ids.InstructionId,
            name
        )

    let mutable videosSequence = [||]
    let mutable instructionSequence = [||]

    medias
    |> Array.iter (fun (mediaContent,newName) ->
                        match mediaContent with
                        | NewAdd.Types.Video vid ->
                            videosSequence <- Array.append videosSequence [|(vid,newName)|]
                        | NewAdd.Types.InstructionTxt instrctn ->
                            instructionSequence <- (Array.append instructionSequence [|(instrctn,newName)|]))

    Array.zip3 videosSequence instructionSequence [|0..videosSequence |> Array.length |> fun x -> x - 1|]
    |> Array.map (fun ((_,newNameVid),(_,newNameInstrcn),pos) ->
                {
                    Title = "Please_provide_Title" + (pos |> string)
                    InstructionVideo = fullPath newNameVid
                    InstructionTxt =  fullPath newNameInstrcn
                })
    |> fun parts ->
            {
                Title =
                    "Please_provide_Title"
                    |> InstructionTitleInfo.HasOldName
                Data = parts
            }
    |> function
        | res when instruction2Add.IsSome ->
            let newParts =
                instruction2Add.Value.Data
                |> Array.append res.Data

            {
                Title = instruction2Add.Value.Title
                Data = newParts
            },ids.InstructionId
        | res -> res,ids.InstructionId
    |> Instruction.Types.NewInstruction2Show
    |> User.Types.InstructionMsg
    |> fun x ->
        let turnToModMode =
            style.visibility.visible
            |> Instruction.Types.ModifyInstructionMsg
            |> fun msg1 ->
                [|
                    msg1
                    (false |>
                     Instruction.Types.DeleteButtonEnablenMsg)
                |]
            |> Array.map (fun msg -> msg |> User.Types.InstructionMsg)
        let changeToInstructionMsg =
            (UserPage.Instruction, 2000)
            |> User.Types.Delay
            |> (User.Types.ChangePage)
        let msgs =
            turnToModMode
            |> Array.append(
                    [|
                        x
                        changeToInstructionMsg
                    |]
                )
        msgs

let saveAsync ( (file,newName) : (Types.File * string) )
              ( options : DatabaseNewFilesOptions )
              ( utils : Data.Utilities<User.Types.Msg> )
              ( ids : DBIds ) = async{

    let fullPath =
        String.Format(
            "User_{0}/Instruction_{1}/{2}",
            ids.UserId,
            ids.InstructionId,
            newName
        )

    let fData =
        FormData.Create()

    let checkSavingMsg =
        (ids,utils,options)
        |> Instruction.Types.CheckIfSaveFinished
        |> User.Types.InstructionMsg

    fData.append("filePath", fullPath)
    fData.append("file", file)

    do! Async.Sleep 3000

    let uploadFinished ( response : Data.SocketEventFinished ) = 
        match response.Status with
        | 200 ->
            let newStatus =
                (
                    fullPath,
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
                            str response.Msg
                        ]
                    ]
                )
                |> Instruction.Types.UploadOrDeleteFinishedSuccesfully

            newStatus
            |> funcChainingIsUploading utils
            |> fun x ->
                [|
                    x
                    checkSavingMsg
                |]
                |> Array.map (fun msg -> msg |> Cmd.ofMsg)
                |> Cmd.batch
                |> User.Types.CmdMsging
        | _ ->
            let msg =
                ("file \"" + file.name + "\" failed with status code: " +
                 ( response.Status |> string ) + response.Msg)
            let newStatus =
                (
                    fullPath,
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
            
            newStatus
            |> funcChainingIsUploading utils
            |> fun x ->
                [|
                    x
                    checkSavingMsg
                |]
                |> Array.map (fun msg -> msg |> Cmd.ofMsg)
                |> Cmd.batch
                |> User.Types.CmdMsging

    let request = async{
        let xhr = XMLHttpRequest.Create()
        xhr.``open``(method = "POST", url = "http://localhost:3001/upload")

        xhr.send(fData) |> fun  _ -> ()
    }

    let mutable time = 0
    let mutable exitLoop = false

    let socketResponse = ProgressSocket.connect("http://localhost:3001")
    
    match socketResponse.ErrorMessage with
    | None  ->
        socketResponse.Socket.Value
        |> ProgressSocket.addEventListener_message(fun scktMsg ->
            let eventResult = (scktMsg :?> Data.SocketEventMessage)
            if eventResult.Path = fullPath
            then
                let newStatus =
                    (fullPath,Instruction.Types.Uploaded.Percentage(eventResult.Progress.percentage))
                    |> Instruction.Types.PartStatus.Uploading
                            
                newStatus
                |> funcChainingIsUploading utils
                |> utils.MsgDispatch) "message"
        |> ProgressSocket.addEventListener_message(fun scktMsg ->
            let response = (scktMsg :?> Data.SocketEventFinished)
            if response.Path = fullPath
            then
                socketResponse.Socket.Value 
                |> ProgressSocket.disconnect
                |> ignore

                exitLoop <- true

                response
                |> uploadFinished
                |> utils.MsgDispatch
            ) "finished"
        |> ignore

    | Some error ->
        exitLoop <- true
        let response =
            {
                Status = 404
                Msg = error
                Path = ""
            }
        response
        |> uploadFinished
        |> utils.MsgDispatch

    do! request

    while time < 15000 && exitLoop = false  do
        do! Async.Sleep 10
        time <- time + 10
        if time > 15000
        then
            let response =
                {
                    Status = 404
                    Msg = "Connectioni timed out"
                    Path = ""
                }
            response
            |> uploadFinished
            |> utils.MsgDispatch

    return User.Types.MsgNone
}

let saveUserData
        ( status : SaveDataProgress<((Types.File * string) * DBIds * Utilities<User.Types.Msg> * DatabaseNewFilesOptions),
                                        array<DatabaseSavingOptions> * DBIds * Utilities<'a>>) =
    match status with 
    | SavingHasNostStartedYet((file,newName),dbIds,utils,options) ->
        (fullPath newName dbIds,Instruction.Types.Uploaded.NoneUploaded)
        |> Instruction.Types.Uploading 
        |> funcChainingIsUploading utils
        |> Cmd.ofMsg
        |> fun x ->
            let funcChainingSavingInProgress info =
                info |>
                (
                    SavingInProgress >>
                    NewAdd.Types.CreateNewDataMsg >>
                    User.Types.NewAddMsg
                )
            [|
                x
                ((file,newName),dbIds,utils,options)
                |> funcChainingSavingInProgress
                |> Cmd.ofMsg
            |]

    | SavingInProgress(media,dbIds,utils,options) ->
          
        dbIds
        |> saveAsync media options utils
        |> Cmd.fromAsync
        |> fun x -> [|x|]

    | SavingResolved(savingOptions,ids,positions) ->
        let dbMsg =
            (savingOptions,ids,positions)
            |> Instruction.Types.DatabaseChangeBegun
            |> Instruction.Types.Msg.DatabaseChangeMsg
            |> User.Types.InstructionMsg
            |> delayedMessage 3000
            |> Cmd.fromAsync
            |> fun x -> [|x|]

        dbMsg

let loadUserItems user password dispatch = async {
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
                        jsonDecodingUser (response.responseText) dispatch
        
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
                         ( password : string Option )
                         ( dispatch : User.Types.Msg -> unit ) =
    ()
    |> function
        | _ when username.IsNone ->
            LoadedUsers (Finished (Error ("Invalid user name")))
            |> Cmd.ofMsg
        | _ when password.IsNone ->
            LoadedUsers (Finished (Error ("Invalid password")))
            |> Cmd.ofMsg
        
        | _ ->
             loadUserItems username.Value password.Value dispatch
             |> Cmd.fromAsync

let getUserDataUpdate ( userData : Data.Deferred<Result<Data.UserData, string>> ) =
    match userData with
    | Data.Deferred.HasNostStartedYet -> [|"" |> User.Types.LoginMessages|] 
    | Data.Deferred.InProgress -> [|"Loading User Data" |> User.Types.LoginMessages|] 
    | Data.Deferred.Resolved response ->
        match response with
        | Ok result ->
            let successMessage =
                
                    [|
                        ("received query with " +
                         (result.Instructions |> Array.length |> string) +
                         " instructions :)" |> User.Types.LoginMessages)
                    |]
            successMessage
            |> Array.append (loadInitData result)

            

        | Error err ->[| err |> User.Types.LoginMessages|]

let existOrNot compareVal result =
     match compareVal with
     | Valid str ->
         result
         |> Array.tryFind (fun usrName -> usrName.Username = str)
         |> function
            | res when res <> None ->
                     Some res.Value
            | _ -> None
     | Invalid ->
         None

let loginAttempt ( status : Data.DeferredWithDispatch<Msg -> unit,Result<LoginInfo * (Msg -> unit), string>> ) =
    match status with
    | DeferredWithDispatch.HasNostStartedYet dispatch ->
        [|
            style.visibility.visible |> User.Types.LoginSpinnerMsg
            dispatch |>
            (
                Started >>
                User.Types.LoadedUsers 
            )
        |]
        
        
    | DeferredWithDispatch.InProgress dispatch ->
        [|"loading user" |> User.Types.LoginMessages|]
        
    | DeferredWithDispatch.Resolved response ->
        let spinnerMessage =
            style.visibility.hidden |> LoginSpinnerMsg

        match response with
        | Ok (loginInfo,dispatch) ->
            [|
                    loginInfo.Id |> NewUserId
                    loginInfo.Id |> (Part.Types.NewUserIdMsg >>
                                     Instruction.Types.PartMsg >>
                                     User.Types.InstructionMsg)
                    dispatch |>
                    (
                        DispatchDefined >>
                        User.Types.Msg.GetUserDispatchMsg
                    )
                    User.Types.LoadedInstructions AsyncOperationEvent.Started
            |]       
        | Error err -> [|err|> User.Types.LoginMessages ; spinnerMessage|]
        
let validateLoginInfo info =
    info
    |> function
       | _ when info = "" ->
            Invalid
       | _ ->
            Valid info

let createNewInstructionId id userData =
    userData.Instructions
    |> Array.length
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
    |> Array.map (fun media ->
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
    |> Array.choose id
    |> Array.map (fun file -> divWithFileName file)
    |> fun elements ->
        Seq.append [ str "Files chosen are:" ; Html.br[] ; Html.br[]] elements   

let currFilesInfo ( filesOption : Option<array<NewAdd.Types.MediaChoiceFormData>> )
                    name =

    match filesOption with
    | Some files ->
        files
        |> Array.filter (fun media ->
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
            | res when (res |> Array.length) = 0 ->
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

let removeOldOfSame ( medias : array<NewAdd.Types.MediaChoiceFormData> )
                      name =
    name
    |> function
        | res when res = "videos" ->
            medias
            |> Array.filter (fun media ->
                match media with
                | NewAdd.Types.Video _ ->
                    false
                | NewAdd.Types.InstructionTxt _ ->
                    true)
        | _ ->
            medias
            |> Array.filter (fun media ->
                match media with
                | NewAdd.Types.Video _ ->
                    true
                | NewAdd.Types.InstructionTxt _ ->
                    false)


let extractMedia ( medias : Option<array<NewAdd.Types.MediaChoiceFormData>> )
                 ( newVids : array<NewAdd.Types.MediaChoiceFormData> )
                   name =
    match medias with
    | Some vids->
        newVids
        |> Array.append (removeOldOfSame vids name)
    | None -> newVids

let decideIfRightFormat ( medias : array<NewAdd.Types.MediaChoiceFormData>) =
    medias
    |> Array.filter (fun media ->
        match media with
        | NewAdd.Types.Video vid ->
            vid.``type`` <> "video/mpeg" &&
            vid.``type`` <> "video/ogg" &&
            vid.``type`` <> "video/mp4" &&
            vid.``type`` <> "video/avi"

        | NewAdd.Types.InstructionTxt instrctn ->
            instrctn.``type`` <> "text/plain")
    |> function
        | res when ( res |> Array.length ) = 0 ->
            None
        | res ->
            let initialMessage =
                [|
                    divWithStyle
                        None
                        "The following files did not have the the right file type:"
                        (prop.style[
                                    style.color.black
                                    style.fontWeight.bold
                                    style.fontSize 13
                        ] )
                |]
            let secondMessage = 
                res
                |> Array.map (fun media ->
                    match media with
                    | NewAdd.Types.Video vid ->
                        [|
                            divWithStyle
                                None
                                vid.name
                                (prop.style[
                                    style.color.indianRed
                                    style.fontWeight.bold
                                    style.fontSize 13
                                ])
                        |] 

                    | NewAdd.Types.InstructionTxt instrctn ->
                        [|
                            divWithStyle
                                None
                                instrctn.name
                                (prop.style[
                                    style.color.indianRed
                                    style.fontWeight.bold
                                    style.fontSize 13
                                ])
                        |])
                |> Array.collect (fun components -> components)
            let finalMessage =
                [|
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
                |]

            finalMessage
            |> Array.append secondMessage
            |> Array.append initialMessage
            |> Some
                 

let decideIfUploadableByTypeCount ( medias : array<NewAdd.Types.MediaChoiceFormData>) =
    let mutable videos = [||]
    let mutable instructions = [||]
    medias
    |> Array.iter (fun media ->
        match media with
        | NewAdd.Types.Video vid ->
            videos <- videos |> Array.append [|vid|]
        | NewAdd.Types.InstructionTxt instrctn ->
            instructions <- instructions |> Array.append [|instrctn|])

    ()
    |> function
        | _ when ( videos |> Array.length ) = ( instructions |> Array.length ) ->
            None
        | _ ->
            [|
                divWithStyle
                    None
                    "צריך לבחור אותו כמות של קבצי וידאו ומארקדבן"
                    (prop.style[
                        style.color.indianRed
                        style.fontWeight.bold
                        style.fontSize 13
                    ])
            |]
            |> Some

let provideNewAddPopUpWait ( utils : Data.Utilities<User.Types.Msg> )
                             wait
                             msgs =
    let newPage =
        ( Global.UserPage.Instruction,wait)
        |> Delay

    let funcChaining newPage msgs =
        (msgs,newPage,utils) |>
        (
            User.Types.PopUpSettings.DefaultNewPage >>
            Some >>
            User.Types.PopUpMsg 
        )
    msgs
    |> funcChaining newPage

let provideNewAddPopUp ( utils : Data.Utilities<User.Types.Msg> )
                         msgs =
    let funcChaining msgs =
        (msgs,utils) |>
        (
            User.Types.PopUpSettings.DefaultWithButton >>
            Some >>
            User.Types.PopUpMsg 
        )
    msgs
    |> funcChaining

let decideIfUploadValid ( medias : array<NewAdd.Types.MediaChoiceFormData>)
                        ( ev : Types.MouseEvent )
                        ( model : NewAdd.Types.Model )
                          dispatch =

    let utils =
        {
            Ev = ev
            MsgDispatch = dispatch
        }
    [|decideIfUploadableByTypeCount medias|]
    |> Array.append [|decideIfRightFormat medias|]
    |> Array.filter (fun msgs ->
        match msgs with
        | Some _ -> true
        | None -> false)
    |> function
        | res when ( res |> Array.length = 0 ) ->
            [|
                divWithStyle
                    None
                    "Shortly you'll be directed to modify the instruction"
                    (prop.style[
                        style.color.black
                        style.fontWeight.bold
                        style.fontSize 15
                    ])
            |]
            |> provideNewAddPopUpWait utils 2000
            |> fun x ->
                        match model.CurrentInstruction with
                        | Some instrOptWId ->
                            let instructionCreationMsg =
                                let instropt,_ =
                                    instrOptWId

                                (medias,instropt)
                                |> GetIdsForNewInstrUpload 
                            [|
                                instructionCreationMsg
                                x
                            |]
                            |> Array.iter (fun msg -> (msg |> dispatch))
                        | _ -> ()
        | res ->
            let utils =
                {
                    Ev = ev
                    MsgDispatch = dispatch
                }
            res
            |> Array.collect (fun msgs -> msgs.Value)
            |> provideNewAddPopUp utils
            |> dispatch

let isUploadable ( model : NewAdd.Types.Model )
                 ( ev : Types.MouseEvent )
                   dispatch =

    let utils =
        {
            Ev = ev
            MsgDispatch = dispatch
        }
    match model.NewInstructionData with
    | Some res ->
        res
        |> function
            | _ when res |> Array.length = 0 ->
                [|
                    divWithStyle
                        None
                        "לא היה הבחרת קביצה, ומספר זהות לא קיימת"
                        (prop.style[
                            style.color.indianRed
                            style.fontWeight.bold
                            style.fontSize 13
                        ])
                |]
                |> fun x ->
                    x
                    |> provideNewAddPopUp utils
                    |> dispatch
            | _ when res |> Array.length = 0 ->
                [|
                    divWithStyle
                        None
                        ("לא היה הבחרת קביצה")
                        (prop.style[
                            style.color.indianRed
                            style.fontWeight.bold
                            style.fontSize 13
                        ])
                |]
                |> fun x ->
                    x
                    |> provideNewAddPopUp utils
                    |> dispatch
            | _  ->
                decideIfUploadValid res ev model dispatch
    | None ->
        [|
            divWithStyle
                None
                "לא היה הבחרת קביצה"
                (prop.style[
                    style.color.indianRed
                    style.fontWeight.bold
                    style.fontSize 13
                ])
        |]
        |> fun x ->
            x
            |> provideNewAddPopUp utils
            |> dispatch

let fileHandle ( ev : Types.Event)
               ( infoOpt : option<Option<Data.InstructionData> * string>  )
                 name
                 dispatch =
    let files = (ev.target?files : Types.FileList)

    infoOpt
    |> function
        | _ when infoOpt.IsSome ->
            [|0..files.length - 1|]
            |> Array.map (fun pos -> chooseMediaByName name
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
    | PopUpSettings.DefaultWithOptions (divs,utils,msgs) ->
        let buttonSettings =
            [|
                Feliz.style.margin 30
                Feliz.style.backgroundColor "grey"
                Feliz.style.fontSize 18
                Feliz.style.borderRadius 10
            |]

        let popupNoMsg =
            (
                {
                    Style = utils.Ev |> ( getPositions >> defaultStyle )
                    ButtonSettings = buttonSettings |> Some
                    ClickMessages = msgs |> Some
                    Messages = divs
                },
                Cmd.none
            )
            |> Some

        popupNoMsg
    | DefaultWithButton (str,utils) ->
        let style = utils.Ev |> ( getPositions >> defaultStyle )

        let buttonSettings =
            [|
                Feliz.style.margin 30
                Feliz.style.backgroundColor "grey"
                Feliz.style.fontSize 18
                Feliz.style.borderRadius 10
            |]


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

    | Default (str,utils) ->
        
        let style = utils.Ev |> ( getPositions >> defaultStyle )

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

    | OptionalWithMsg (divs,utils,styles) ->
        let positions =
            getPositions utils.Ev
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

    | DefaultNewPage (divs,newPage,utils) ->
        let style = utils.Ev |> ( getPositions >> defaultStyle )
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
                          ( instructionInfo : array<Instruction.Types.modificationInfo> Option )
                            userDataInstructions =

    let compareWithExistingInstruction newInstructionPerhapsNotOldName =
        let getFoundAlreadyExistingInstruction newInstructionTitle =
            userDataInstructions
            |> Array.indexed
            |> Array.tryPick (fun (pos,existInstr) ->
                match newInstructionTitle with
                | Data.InstructionTitleInfo.HasOldName title ->
                    match existInstr.Title with
                    | Data.InstructionTitleInfo.HasOldName titleFromDataBase ->
                        let sameOldName = title.Replace(" ", "") = titleFromDataBase.Replace(" ", "")
                        if sameOldName
                        then
                            {| SameOldName = sameOldName ; SameNewName = false ; ExistInstruction = existInstr ; Position  = pos|}
                            |> Some
                        else None
                        
                    | Data.InstructionTitleInfo.HasNewName titles -> 
                        let sameOldName = title.Replace(" ", "") = titles.OldName.Replace(" ", "")

                        if sameOldName
                        then
                            {| SameOldName = sameOldName ; SameNewName = false ; ExistInstruction = existInstr ; Position  = pos|}
                            |> Some
                        else None
                | Data.InstructionTitleInfo.HasNewName titles ->
                    match existInstr.Title with
                    | Data.InstructionTitleInfo.HasOldName titleFromDataBase ->
                        let sameOldName = titleFromDataBase.Replace(" ", "") = titles.OldName.Replace(" ", "") 
                        let sameNewName = titleFromDataBase.Replace(" ", "") = titles.NewName.Replace(" ", "")

                        if sameOldName
                        then
                            {| SameOldName = sameOldName ; SameNewName = sameNewName ; ExistInstruction = existInstr ; Position  = pos|}
                            |> Some
                        else None


                    | Data.InstructionTitleInfo.HasNewName titlesExisting -> 
                        let sameOldName = titlesExisting.OldName.Replace(" ", "") = titles.OldName.Replace(" ", "")
                        let sameNewName = titlesExisting.OldName.Replace(" ", "") = titles.NewName.Replace(" ", "")

                        if sameOldName
                        then
                            {| SameOldName = sameOldName ; SameNewName = sameNewName ; ExistInstruction = existInstr ; Position  = pos|}
                            |> Some
                        else None)

        let foundAlreadyExistingInstruction =
            newInstructionPerhapsNotOldName.Title
            |> getFoundAlreadyExistingInstruction

        let newInstruction =
            match newInstructionPerhapsNotOldName.Title with
            | Data.InstructionTitleInfo.HasOldName _ ->
                newInstructionPerhapsNotOldName
            | Data.InstructionTitleInfo.HasNewName titles ->
                let newTitle =
                    titles.NewName
                    |> InstructionTitleInfo.HasOldName

                { newInstructionPerhapsNotOldName with Title = newTitle}


        foundAlreadyExistingInstruction            
        |> function
            | res when res.IsSome ->
                let (pos,existingInstr) =
                    (res.Value.Position,res.Value.ExistInstruction)
                let instrId =
                    pos |> string

                let allTitlesUnique =
                    newInstruction.Data
                    |> Array.map (fun newPart ->
                        newInstruction.Data
                        |> Array.filter (fun newPartCompare ->
                                let result =
                                    newPart.Title.Replace(" ", "") =
                                        newPartCompare.Title.Replace(" ", "")
                                result)
                        |> function
                            | res when res |> Array.length = 1 ->
                                None
                            | _ -> Some newPart.Title 
                            )
                    |> Array.choose id
                    |> function
                        | res when res |> Array.length <> 0 ->
                            res
                            |> Array.distinct
                            |> Array.map (fun title ->
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
                                existingInstr.Data |> Array.length =
                                    (newInstruction.Data |> Array.length) 
                                 

                            let newAndOldInstructionHaveSameParts =
                                newInstruction.Data
                                |> Array.forall (fun newPart ->
                                    existingInstr.Data
                                    |> Array.exists (fun part ->
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
                            newAndOldInstructionHaveSameParts &&
                            foundAlreadyExistingInstruction.Value.SameNewName
                        ()
                        |> function
                            | _ when alreadyExistingInstruction = true ->
                                "Nothing to save!"
                                |> User.Types.newSaveResult.ThatInstructionAlreadyExists
                            | _ ->
                                let newFileParts =
                                    newInstruction.Data
                                    |> Array.choose (fun newPart ->
                                        existingInstr.Data
                                        |> Array.forall (fun part ->
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
                                        | partsWNewTitles when partsWNewTitles |> Array.length <> 0 ->
                                            partsWNewTitles |> Some
                                        | _ -> None
                                let partsWithNewNames =
                                    newInstruction.Data
                                    |> Array.choose (fun newPart ->
                                        existingInstr.Data
                                        |> Array.tryFind (fun part ->
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
                                        | partsWNewTitles when partsWNewTitles |> Array.length <> 0 ->
                                            partsWNewTitles |> Some
                                        | _ -> None

                                let partsToDelete =
                                    existingInstr.Data
                                    |> Array.choose (fun part ->
                                        newInstruction.Data
                                        |> Array.tryFind (fun partNew ->

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
                                        | partsToDelete when partsToDelete |> Array.length <> 0 ->
                                            Some partsToDelete
                                        | _ -> None
                                ()
                                |> function
                                    | _ when partsWithNewNames.IsSome &&
                                                newFileParts.IsSome &&
                                                partsToDelete.IsSome ->
                                            let info =
                                                [|
                                                    { newInstruction with Data = partsWithNewNames.Value }
                                                    |> DatabaseSavingOptions.NewNameInstruction

                                                    { newInstruction with Data = newFileParts.Value }
                                                    |> DatabaseNewFilesOptions.SameInstructionOption
                                                    |> DatabaseSavingOptions.NewFilesInstruction

                                                    
                                                    { newInstruction with Data = partsToDelete.Value } |>
                                                    (DatabaseDeleteOptions.DeleteParts >>
                                                     DatabaseSavingOptions.PartsToDeleteInstruction)
                                                |]

                                            (info,instrId)
                                            |> User.Types.newSaveResult.SaveExistingNewFilesAndTItlesPartsToDelete

                                    | _ when partsWithNewNames.IsSome &&
                                             newFileParts.IsSome ->
                                             let info =
                                                 [|
                                                     { newInstruction with Data = newFileParts.Value }
                                                     |> DatabaseNewFilesOptions.SameInstructionOption
                                                     |> DatabaseSavingOptions.NewFilesInstruction

                                                     { newInstruction with Data = partsWithNewNames.Value }
                                                     |> DatabaseSavingOptions.NewNameInstruction
                                                 |]

                                             (info,instrId)
                                             |> User.Types.newSaveResult.SaveExistingNewFilesAndTItles
                                    | _ when newFileParts.IsSome &&
                                             partsToDelete.IsSome ->
                                            let info =
                                                [|
                                                    { newInstruction with Data = newFileParts.Value }
                                                    |> DatabaseNewFilesOptions.SameInstructionOption
                                                    |> DatabaseSavingOptions.NewFilesInstruction

                                                    { newInstruction with Data = partsToDelete.Value } |>
                                                    (DatabaseDeleteOptions.DeleteParts >>
                                                     DatabaseSavingOptions.PartsToDeleteInstruction)
                                                |]

                                            (info,instrId)
                                            |> User.Types.newSaveResult.SaveExistingNewFilesPartsToDelete
                                    | _ when partsWithNewNames.IsSome &&
                                             partsToDelete.IsSome ->
                                            let info =
                                                [|
                                                    { newInstruction with Data = partsWithNewNames.Value }
                                                    |> DatabaseSavingOptions.NewNameInstruction

                                                    { newInstruction with Data = partsToDelete.Value } |>
                                                    (DatabaseDeleteOptions.DeleteParts >>
                                                     DatabaseSavingOptions.PartsToDeleteInstruction)
                                                |]

                                            (info,instrId)
                                            |> User.Types.newSaveResult.SaveExistingNewTItlesPartsToDelete
                                    | _ when newFileParts.IsSome ->
                                        let info =
                                            [|
                                                { newInstruction with Data = newFileParts.Value }
                                                |> DatabaseNewFilesOptions.SameInstructionOption
                                                |> DatabaseSavingOptions.NewFilesInstruction
                                            |]

                                        (info,instrId)
                                        |> User.Types.newSaveResult.SaveExisitngNewFIles
                                    | _ when partsWithNewNames.IsSome ->
                                        let info =
                                            [|
                                                { newInstruction with Data = partsWithNewNames.Value }
                                                |> DatabaseSavingOptions.NewNameInstruction
                                            |]

                                        (info,instrId)
                                        |> User.Types.newSaveResult.SaveExistingNewTitles
                                    | _ ->
                                        let info =
                                            [|
                                                { newInstruction with Data = partsToDelete.Value } |>
                                                (DatabaseDeleteOptions.DeleteParts >>
                                                 DatabaseSavingOptions.PartsToDeleteInstruction)
                                            |]

                                        (info,instrId)
                                        |> User.Types.newSaveResult.SaveExistingPartsToDelete
            | _ ->
                let instrid =
                    userDataInstructions
                    |> Array.indexed
                    |> Array.last
                    |> fun (pos,_) ->
                        pos + 1
                        |> string
                (newInstruction,instrid)
                |> User.Types.newSaveResult.SaveNew

                
    match instructionInfo with
    | Some modInfos ->
        instruction.Data
        |> Array.map (fun part ->
            modInfos
            |> Array.tryFind (fun modInfo ->
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
        |> Array.choose id 
        |> function
            | res when res |> Array.length <> 0 ->
                let newinstruction =
                    { instruction with Data = res }

                let msg =
                    newinstruction
                    |> compareWithExistingInstruction

                match msg with
                | newSaveResult.SaveNew _ ->
                    (msg, User.Types.UpdateUserInstructionsType.AddNewInstruction(newinstruction) |> Some)
                | _ ->
                    (msg, User.Types.UpdateUserInstructionsType.UpdateInstruction(newinstruction) |> Some)
                
            | _ ->
                let msg =
                    ("You are attempting to save an empty instruction.
                    Click the delete button if you wish to delete the instruction")
                    |> User.Types.newSaveResult.InstructionIsDelete

                (msg,None)
                
    | _ ->
        let msg =
            "No media has been loaded, re-upload your shit"
            |> User.Types.newSaveResult.NoUserData

        (msg,None)


let savingChoices userDataOpt ( utils : Utilities<User.Types.Msg> ) instruction instructionInfo =
    match userDataOpt with
    | Resolved( Ok data) ->
        let (result,possibleNewInstruction) =
            savingChoicesTestable instruction
                                  instructionInfo
                                  data.Instructions

        let newStatus statusMsg =
            [|
                Html.div[
                    prop.className "column"
                    prop.style[
                        style.color.red
                    ]
                    prop.children[
                        Fable.React.Helpers.str statusMsg
                    ]
                ]
            |]

        let funcChainingOptions positions popupMsg msgs =
            
            (popupMsg |> newStatus,positions,msgs) |>
            (
                PopUpSettings.DefaultWithOptions >>
                Some >>
                User.Types.PopUpMsg >>
                Cmd.ofMsg
            )

        let funcChaining utils popupMsg =
            
            (popupMsg |> newStatus,utils) |>
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
                utils
            )
            |> (NewAdd.Types.Msg.NewDataToInstruction >>
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
                |> funcChaining utils
                |> Cmd.ofMsg

            let dbMsg =
                (savingOptions,dbIds,utils)|>
                (
                    Instruction.Types.DatabaseChangeBegun >>
                    Instruction.Types.Msg.DatabaseChangeMsg >>
                    User.Types.InstructionMsg >>
                    Cmd.ofMsg
                )


            [|
                popupMsg
                dbMsg
            |]
            |> Cmd.batch
            |> User.Types.Msg.CmdMsging

        let createNewSaveAndDBChangeMsgs savingOptions instrId =
            let newInstr =
                savingOptions
                |> Array.tryPick (fun opt ->
                    match opt with
                    | DatabaseSavingOptions.NewFilesInstruction newInstr ->
                        Some newInstr
                    | _ -> None)

            let onlyDbChange =
                savingOptions
                |> Array.choose (fun opt ->
                    match opt with
                    | DatabaseSavingOptions.NewFilesInstruction _ ->
                        None
                    | _ -> Some opt)
                |> function
                    | onlyDbChange when onlyDbChange |> Array.length <> 0 ->
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

                    [|
                        newSaveMsg
                        dbMsg
                    |]
                    |> Cmd.batch
                    |> User.Types.CmdMsging
                    |> fun x -> [|x|]

                | _ when newInstr.IsSome ->

                    let newSaveMsg =
                        instrId
                        |> saveNewMsg newInstr.Value

                    newSaveMsg
                    |> fun x -> [|x|]

                | _ when onlyDbChange.IsSome ->

                    let dbMsg =
                        onlyDbChange.Value
                        |> getDatabaseMsg instrId

                    dbMsg
                    |> fun x -> [|x|]

                | _ -> [|User.Types.Msg.MsgNone|]
            
        let msg =
            match result with
            | SaveNew (newInstr,instrId) ->
                let popupMsg =
                    "Are you sure you want to save a new instruction?"

                let savingOptions =
                    newInstr
                    |> DatabaseNewFilesOptions.NewInstructionOption
                    |> DatabaseSavingOptions.NewFilesInstruction
                    |> fun x -> [|x|]

                createNewSaveAndDBChangeMsgs savingOptions instrId
                |> funcChainingOptions utils popupMsg
            | SaveExistingNewTitles (savingOptions,instrId) ->
                let popupMsg =
                    "Are you sure you want to save an existing instruction with new titles?"

                createNewSaveAndDBChangeMsgs savingOptions instrId
                |> funcChainingOptions utils popupMsg
            | SaveExisitngNewFIles (savingOptions,instrId) ->
                let popupMsg =
                    "Are you sure you want to save existing instruction with new files?"

                createNewSaveAndDBChangeMsgs savingOptions instrId
                |> funcChainingOptions utils popupMsg
                
            | SaveExistingNewFilesAndTItles (savingOptions,instrId) ->
                let popupMsg =
                    "Are you sure you want to save existing instruction with new files and titles?"

                createNewSaveAndDBChangeMsgs savingOptions instrId
                |> funcChainingOptions utils popupMsg
            | SaveExistingNewFilesPartsToDelete (savingOptions,instrId) ->
                let popupMsg =
                    "Are you sure you want to save existing instruction with new files?"

                createNewSaveAndDBChangeMsgs savingOptions instrId
                |> funcChainingOptions utils popupMsg
            | SaveExistingNewTItlesPartsToDelete (savingOptions,instrId) ->
                let popupMsg =
                    "Are you sure you want to save existing instruction with new part titles?"

                createNewSaveAndDBChangeMsgs savingOptions instrId
                |> funcChainingOptions utils popupMsg
            | SaveExistingNewFilesAndTItlesPartsToDelete (savingOptions,instrId) ->
                let popupMsg =
                    "Are you sure you want to save existing instruction with new files and titles?"

                createNewSaveAndDBChangeMsgs savingOptions instrId
                |> funcChainingOptions utils popupMsg
            | SaveExistingPartsToDelete (savingOptions,instrId) ->
                let popupMsg =
                    "Are you sure you want to save the changes?"

                createNewSaveAndDBChangeMsgs savingOptions instrId
                |> funcChainingOptions utils popupMsg
            | InstructionIsDelete errorMsg ->
                errorMsg
                |> funcChaining utils
                |> Cmd.ofMsg
            | NoUserData errorMsg ->
                errorMsg
                |> funcChaining utils
                |> Cmd.ofMsg
            | ThatInstructionAlreadyExists errorMsg ->
                errorMsg
                |> funcChaining utils
                |> Cmd.ofMsg
            | InstructionHasNotDistinctTitles errorMsg ->
                errorMsg
                |> funcChaining utils
                |> Cmd.ofMsg
        ()
        |> function
            | _ when possibleNewInstruction.IsSome ->
                [|
                    possibleNewInstruction.Value
                    |> User.Types.Msg.PossibleNewUserDataMsg
                    |> Cmd.ofMsg

                    msg
                |]
                |> Cmd.batch
            | _ ->
                msg
    |   _ ->
        []
