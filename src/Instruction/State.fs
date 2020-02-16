module Instruction.State

open Elmish
open Controls
open Types
open Part
open Data
open Feliz
open Fable.Core
open Browser

let init () : Model * Cmd<Msg> =

    {
      InstructionErrorMessage = defaultAppearanceAttributes
      CurrInstruction = Error ""
      CurrPart = Part.State.init() |> fun (a,b) -> a
      CurrPositions = None
      PartNameModificationInput =
        { defaultAppearanceAttributes with Visible = style.visibility.hidden }
      PositionsInput = defaultAppearanceAttributes
      DeleteButton =
        { defaultAppearanceAttributes with Disable = true }
    }, []


let update msg model : Instruction.Types.Model * Cmd<Msg>  =
    match msg with
    | NewInstruction2Show instruction ->
        { model with CurrInstruction = Ok instruction }, []
    | PartMsg msg ->
        let (parModel, partModelCmd) = Part.State.update msg model.CurrPart
        { model with CurrPart = parModel}, Cmd.map PartMsg partModelCmd
    | ErrorMsg str ->
        { model with InstructionErrorMessage =
                        { model.InstructionErrorMessage with Text = str} }, []
    | ModifyInstructionMsg visibility ->
        console.log(visibility)
        { model with PartNameModificationInput =
                        { model.PartNameModificationInput with Visible = visibility } }, []
    | DeleteButtonEnablenMsg isDisabled ->
        console.log(isDisabled)
        { model with DeleteButton =
                        { model.DeleteButton with Disable = isDisabled } }, []
    | NewModificationInfo (posOpt,checkedOpt,nameOpt) ->
        let changePositions ( sequence : seq<'t> ) ( posPair : int * int ) =
            let len =
                (sequence |> Seq.length)
                |> function
                    | res when res < 1 ->
                        0
                    | res -> res - 1

            let Arr = sequence |> Seq.toArray       
            let posOne = posPair |> fun (x,_) -> x
            let posTwo = posPair |> fun (_,y) -> y
            ()
            |> function
                | _ when posOne = posTwo || len = 0 ->
                    sequence
                | _ when posOne = 0 && posTwo = len ->
                    let first = Arr.[len]
                    let second = Arr.[1..len - 1]
                    let third = Arr.[posOne]

                    seq[first]
                    |> Seq.append (second |> Array.toSeq)
                    |> Seq.append (seq[third])
                    
                | _ when posOne = len && posTwo = 0 ->
                    let first = Arr.[len]
                    let second = Arr.[1..len - 1]
                    let third = Arr.[posTwo]

                    seq[first]
                    |> Seq.append (second |> Array.toSeq)
                    |> Seq.append (seq[third])

                | _ when posOne = 0 ->
                    let first = Arr.[posTwo]
                    let second = Arr.[1..posTwo - 1]
                    let third = Arr.[posOne]
                    let fourth = Arr.[posTwo + 1..len]

                    seq[first]
                    |> Seq.append (second |> Array.toSeq)
                    |> Seq.append (seq[third])
                    |> Seq.append (fourth |> Array.toSeq)

                | _ when posTwo = 0 ->
                    let first = Arr.[posOne]
                    let second = Arr.[1..posOne - 1]
                    let third = Arr.[posTwo]
                    let fourth = Arr.[posOne + 1..len]

                    seq[first]
                    |> Seq.append (second |> Array.toSeq)
                    |> Seq.append (seq[third])
                    |> Seq.append (fourth |> Array.toSeq)

                | _ when posOne = len ->
                    let first = Arr.[0..posTwo - 1]
                    let second = Arr.[len]
                    let third = Arr.[posTwo + 1..len - 1]
                    let fourth = Arr.[posTwo]

                    first
                    |> Seq.append (seq[second])
                    |> Seq.append third
                    |> Seq.append (seq[fourth])

                | _ when posTwo = len ->
                    let first = Arr.[0..posOne - 1]
                    let second = Arr.[len]
                    let third = Arr.[posOne + 1..len - 1]
                    let fourth = Arr.[posOne]

                    first
                    |> Seq.append (seq[second])
                    |> Seq.append third
                    |> Seq.append (seq[fourth])

                | _ when posOne > 0 &&
                         posOne < len &&
                         posTwo > 0 &&
                         posTwo < len ->
                            ()
                            |> function
                                | _ when posOne < posTwo ->
                                    let first = Arr.[0..posOne - 1]
                                    let second = Arr.[posTwo]
                                    let third = Arr.[posOne + 1..posTwo - 1]
                                    let fourth = Arr.[posOne]
                                    let fifth = Arr.[posTwo + 1..len]

                                    first
                                    |> Seq.append (seq[second])
                                    |> Seq.append third
                                    |> Seq.append (seq[fourth])
                                    |> Seq.append fifth
                                | _ ->
                                    let first = Arr.[0..posTwo - 1]
                                    let second = Arr.[posOne]
                                    let third = Arr.[posTwo + 1..posOne - 1]
                                    let fourth = Arr.[posTwo]
                                    let fifth = Arr.[posOne + 1..len]

                                    first
                                    |> Seq.append (seq[second])
                                    |> Seq.append third
                                    |> Seq.append (seq[fourth])
                                    |> Seq.append fifth
                | _ ->
                    sequence

        let currInst =
            match model.CurrInstruction with
            | Ok instrctn ->
                ()
                |>function
                    | _ when (instrctn.Data |> Seq.length = 0) ->
                        None
                    | _ ->
                        Some instrctn
            | _ ->
                None

        let defaultValueToReturn = model, []

        nameOpt
        |> function
            | res when currInst.IsNone ->
                let instructionLen = currInst.Value.Data |> Seq.length |> fun x -> x - 1

                let currPartPositionOpt =
                    currInst.Value.Data
                    |> fun parts ->
                        Seq.zip parts (Global.getPositionSequence parts)
                        |> Seq.tryFind (fun (part,_) -> part.Title = nameOpt.CurrName)
                        |> function
                            | res when res.IsSome ->
                                res.Value |> fun (part,pos) ->
                                    Some pos
                            | _ -> None

                let newIsChecked =
                    match checkedOpt with
                    | Some checkVal -> checkVal
                    | _ -> model.

                ()
                |> function
                    | _ when currPartPositionOpt.IsSome && posOpt.IsSome ->
                        let currPartPosition = currPartPositionOpt.Value
                        let posNew = posOpt.Value
                        let bothAreSame = (currPartPosition = posNew)

                        ()
                        |> function
                            | _ when bothAreSame ->
                            | _ when currPartPosition = 0 ->
                            | _ when posNew = 0 ->
                            | _ when currPartPosition = instructionLen ->
                            | _ when posNew = instructionLen ->
                            | _ when posNew > 0 &&
                                     posNew < instructionLen &&
                                     currPartPosition > 0 &&
                                     currPartPosition < instructionLen ->
                            | _ -> 
        
                    | _ -> defaultValueToReturn

            | _ -> defaultValueToReturn
        
