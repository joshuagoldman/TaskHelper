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
      CurrTempInstruction = None
      PartNameModificationInput =
        { defaultAppearanceAttributes with Visible = style.visibility.hidden }
      PositionsInput = defaultAppearanceAttributes
      DeleteButton =
        { defaultAppearanceAttributes with Disable = true }
      FileAddMsg = defaultAppearanceAttributes
    }, []


let update msg model : Instruction.Types.Model * Cmd<User.Types.Msg>  =
    match msg with
    | NewInstruction2Show instruction ->
        let delOrReg  =
            "Delete"
            |> ( Delete >> Some )
        let newModInfo =
            instruction.Data
            |> Seq.map (fun part ->
                (delOrReg,part.Title,None)
                |> ( NewModificationInfo
                     >> User.Types.InstructionMsg
                     >> Cmd.ofMsg ))
        { model with CurrInstruction = Ok instruction }, Cmd.batch newModInfo
    | PartMsg msg ->
        let (parModel, partModelCmd) = Part.State.update msg model.CurrPart
        { model with CurrPart = parModel}, Cmd.map (PartMsg >> User.Types.InstructionMsg) partModelCmd
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
    | NewModificationInfo (delOrReg,currName,newName) ->
        match model.CurrInstruction with
        | Ok instruction ->
            match model.CurrPositions with
            | Some modinfo ->
                let (newInstruction,newModInfo) =
                    Logic.updateCurrPositionsTestable instruction
                                                      modinfo
                                                      delOrReg
                                                      currName
                                                      newName
                { model with CurrInstruction = Ok newInstruction
                             CurrPositions = Some newModInfo}, []
            | _ -> model,[]
        | _ -> model,[]

    | NewName (currName,newName) ->
        match model.CurrInstruction with
        | Ok instruction ->
            instruction.Data
            |> Seq.map (fun part ->
                ()
                |> function
                    | _ when part.Title = currName  ->
                        { part with Title = newName}
                    | _ -> part)
                |> fun parts ->
                    { instruction with Data = parts}
                    |> fun newInstruction ->
                        { model with CurrInstruction = Ok newInstruction }, []
        | _ -> model,[]

    | UpdateNewName (currName,newName) ->
        match model.CurrPositions with
        | Some currPositions ->
            Instruction.Logic.updateNewNameTestable currPositions
                                                    currName
                                                    newName
            
            |> fun newCurrPos ->
                 { model with CurrPositions = Some newCurrPos }, []
        | _ -> model,[]

    | Reset(ResetInstructionNotObtained str)->
        model, ( str |>
                 ( User.Types.GiveResetInstruction >> Cmd.ofMsg ))
    | Reset(ResetInstructionObtained instruction)->
        { model with CurrInstruction = Ok instruction}, []
        
