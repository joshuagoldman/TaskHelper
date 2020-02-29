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
        { model with CurrInstruction = Ok instruction }, (instruction |>
                                                          ( NewCurrPositions >>
                                                            User.Types.InstructionMsg >>
                                                            Cmd.ofMsg ))
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
    | NewModificationInfo (posOpt,checkedOpt,namePair) ->
        Logic.updateCurrPositions model (posOpt,checkedOpt,namePair)

    | NewCurrPositions instruction ->
        let currPositions =
            Seq.zip instruction.Data (seq[0..instruction.Data |> Seq.length |> fun x -> x - 1])
            |> Seq.map (fun (data,pos) ->
                {
                    Names = {
                        CurrName = data.Title
                        NewName = None
                    }
                    Position = Some pos
                    IsChecked = Some false
                })
            |> Some

        { model with CurrPositions = currPositions }, []

        
