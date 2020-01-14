module Data

open Fable.ReactServer
open Elmish
open Thoth.Json

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

type AsyncOperationEvent<'t> =
    | Started 
    | Finished of 't

let allData (userName : string ) =
    seq
        [
            {
                Title = "Example Instruction"
                Data =
                    seq
                        [
                            {
                                Title = "Example Part 1"
                                InstructionVideo = "ExampleVideo1.mp4"
                                InstructionTxt = "First example video"
                            }

                            {
                                Title = "Example Part 2"
                                InstructionVideo = "ExampleVideo2.mp4"
                                InstructionTxt = "Second example video"
                            }

                            {
                                Title = "Example Part 3"
                                InstructionVideo = "ExampleVideo3.mp4"
                                InstructionTxt = "Third example video"
                            }

                            {
                                Title = "Example Part 4"
                                InstructionVideo = "ExampleVideo4.mp4"
                                InstructionTxt = "Fourth example video"
                            }

                            {
                                Title = "Example Part 5"
                                InstructionVideo = "ExampleVideo5.mp4"
                                InstructionTxt = "Fifth example video"
                            }
                        ]
            }
        ]

