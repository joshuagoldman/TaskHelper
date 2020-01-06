module Data

open Fable.ReactServer
open Elmish
open Thoth.Json

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

let PartDecoder : Decoder<partData> = 
        Decode.object (fun fields -> {
            InstructionVideo = fields.Required.At ["InstructionVideo"] Decode.string

            InstructionTxt = fields.Required.At ["InstructionTxt"] Decode.string
            Title = fields.Required.At ["Title"] Decode.string
        })

let PartlistDecoder =
    Decode.array PartDecoder

let InstructionDecoder : Decoder<InstructionData> =
    Decode.object (fun fields -> {
        Data = fields.Required.At ["Data"] PartlistDecoder
        Title = fields.Required.At ["Title"] Decode.string
    })

let parseInstructionItems (json : string ) =
    Decode.list InstructionDecoder
    |> fun x -> Decode.fromString x json

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

