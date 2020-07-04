module Data

open Fable.ReactServer
open Elmish
open Thoth.Json
open Fable.Core.JsInterop
open Browser
open TaskHelperJsInterop

type Validity =
    | Valid of string
    | Invalid

type ValidationLoginInfo =
    {
        Username : Validity
        Password : Validity
    }

type LoginInfo =
    {
        Username : string
        Password : string
        Id : int
    }

type partData =
    {
        InstructionVideo : string
        InstructionTxt : string
        Title : string
    }


type InstructionName = {
    DbName : string
    OldName : string
    NewName : string
}

type InstructionTitleInfo =
    | HasOldName of string
    | HasNewName of InstructionName


type InstructionData =
    {
        Data : array<partData>
        Title : InstructionTitleInfo
    }

type DatabaseNewFilesOptions =
    | NewInstructionOption of InstructionData 
    | SameInstructionOption of InstructionData

type DatabaseDeleteOptions =
    | DeleteInstruction of InstructionData 
    | DeleteParts of InstructionData

type NewNameOptions =
    | OnlyInstructionNameCHange of InstructionTitleInfo 
    | PartsChangeOrBoth of InstructionData

type DatabaseSavingOptions = 
    | NewFilesInstruction of DatabaseNewFilesOptions
    | NewNameInstruction of NewNameOptions
    | PartsToDeleteInstruction of DatabaseDeleteOptions

type UsrTypeDispatchOptions<'a> =
    | DispatchDefined of ('a -> unit)
    | NoDispatchDefined

type UserData =
    {
        Id : int
        Instructions : array<InstructionData>
    }

let PartDecoder : Decoder<partData> = 
        Decode.object (fun fields -> {
            InstructionVideo = fields.Required.At ["instruction_video"] Decode.string
            InstructionTxt = fields.Required.At ["instruction_txt"] Decode.string
            Title = fields.Required.At ["part_title"] Decode.string
        })

let PartArrayDecoder =
    Decode.array PartDecoder

let InstructionDecoder : Decoder<InstructionData> = 
    Decode.object (fun fields -> {
        Data = fields.Required.At ["parts"] PartArrayDecoder
        Title =
            fields.Required.At ["title"] Decode.string
            |> InstructionTitleInfo.HasOldName
    })

let InstructionArrayDecoder =
    Decode.array InstructionDecoder

let UserDataDecoder : Decoder<UserData> =
    Decode.object (fun fields -> {
        Id = fields.Required.At ["id"] Decode.int
        Instructions = fields.Required.At ["instructions"] InstructionArrayDecoder
    })

let UserDataArrayDecoder =
    Decode.array UserDataDecoder

let parseUserData (json : string ) =
    Decode.fromString UserDataArrayDecoder json

let LoginInfoDecoder : Decoder<LoginInfo> =
    Decode.object (fun fields -> {
        Username = fields.Required.At ["user_name"] Decode.string
        Password = fields.Required.At ["password"] Decode.string
        Id = fields.Required.At ["id"] Decode.int
    })

let LoginInfoArrayDecoder json =
    Decode.fromString (Decode.array LoginInfoDecoder) json

   

module Cmd =
    let fromAsync ( operation : Async<'msg> ) : Cmd<'msg> =
        let delayedCmd ( dispatch: 'msg -> unit ) : unit =
            let delayedDispatch = async {
                    let! msg = operation
                    dispatch msg
             }

            Async.StartImmediate delayedDispatch

        Cmd.ofSub delayedCmd

module Async =
    let map f ( computation: Async<'t> ) =
        async {
            let! x = computation
            return f x
        }

type DeferredWithDispatch<'t,'u> =
    | HasNostStartedYet of 't
    | InProgress of 't
    | Resolved of 'u

type Deferred<'t> =
    | HasNostStartedYet
    | InProgress
    | Resolved of 't

type SaveDataProgress<'a,'b> =
    | SavingHasNostStartedYet of 'a 
    | SavingInProgress of 'a
    | SavingResolved  of 'b

type AsyncOperationSavingStatus<'t,'u> =
    | SavingWillBegin of 't 
    | SavingOnGoing of 'u
    | SavingFinished of 'u

type AsyncOperationEvent<'t> =
    | Started
    | Finished of 't

type AsyncOperationEventWithDispatch<'t,'u> =
    | Started of 't
    | Finished of 'u

type eventWithDIspatch<'t> ={
    ev : Browser.Types.MouseEvent
    dispatch : 't -> unit 
}

type DBIds = {
    UserId : string
    InstructionId : string
}

type Positions = {
    X : float
    Y : float
}

type Utilities<'a> = {
    MsgDispatch : 'a -> unit
    Positions : Positions
}

let errorPart =
        {
            InstructionVideo = ""
            InstructionTxt = "No data found"
            Title  = "No title"
        }

let errorInstruction =
    
        {
            Title =
                ""
                |> InstructionTitleInfo.HasOldName
            Data =
                [|
                    errorPart 
                |]
        }

type SocketEventFinished = {
    Status : int
    Msg : string
    Path : string
}

type SocketEventMessage = {
    Progress : IProgress
    Path : string
}


