module Data

open Fable.ReactServer
open Elmish
open Thoth.Json
open Fable.Core.JsInterop

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

type InstructionData =
    {
        Data : seq<partData>
        Title : string
    }

type DatabaseNewFilesOptions =
    | NewInstructionOption of InstructionData 
    | SameInstructionOption of InstructionData

type DatabaseDeleteOptions =
    | DeleteInstruction of InstructionData 
    | DeleteParts of seq<partData> 

type DatabaseSavingOptions = 
    | NewFilesInstruction of DatabaseNewFilesOptions
    | NewNameInstruction of InstructionData
    | PartsToDeleteInstruction of DatabaseDeleteOptions 

type UserData =
    {
        Id : int
        Instructions : seq<InstructionData>
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
        Title = fields.Required.At ["title"] Decode.string
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

type eventWithDIspatch<'t> ={
    ev : Browser.Types.MouseEvent
    dispatch : 't -> unit 
}

type DBIds = {
    UserId : string
    InstructionId : string
}

type Position = {
    X : float
    Y : float
}

let errorPart =
        {
            InstructionVideo = ""
            InstructionTxt = "No data found"
            Title  = "No title"
        }

let errorInstruction =
    
        {
            Title = ""
            Data =
                seq
                    [
                        errorPart 
                    ]
        }


