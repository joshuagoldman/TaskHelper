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
                let names =
                    {
                        CurrName = part.Title
                        NewName = None
                    }
                {
                    DelOrReg = delOrReg
                    Names = names
                })
        { model with CurrInstruction = Ok instruction ;
                     CurrPositions = Some newModInfo }, []
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

    | ImplementNewNames ->
        match model.CurrInstruction with
        | Ok instruction ->
            let result =
                Logic.implementNewNamesTestable instruction model.CurrPositions
            let kattenJansson =
                match model.CurrPositions with
                | Some modInfo ->
                    modInfo
                    |> Seq.map (fun info ->
                        let delOrReg =
                            match info.DelOrReg with
                            | Some button ->
                                match button with
                                | Delete str -> str
                                | Regret str -> str
                            | _ -> ""
                        info.Names.CurrName +
                        "\n" +
                        (if info.Names.NewName.IsSome then info.Names.NewName.Value else "") +
                        "\n" +
                        delOrReg +
                        "\n\n")
                    |> String.concat ""
                | _ -> "2"
            result
            |> function
                | res when res.IsSome ->
                    let (newInstruction,newModinfo) =
                        res.Value |> fun (a,b) -> (a,b)
                    {model with CurrInstruction = Ok newInstruction ;
                                CurrPositions = Some newModinfo },[]
                | _ -> model, []
        | _ -> model, []

    | UpdateNewName (currName,newName) ->
        match model.CurrPositions with
        | Some currPositions ->
            Instruction.Logic.updateNewNameTestable currPositions
                                                    currName
                                                    newName
            
            |> fun newCurrPos ->
                 { model with CurrPositions = Some newCurrPos }, []
        | _ -> model,[]

    | ResetInstruction str ->
        model, ( str |>
                 ( User.Types.GiveResetInstruction >> Cmd.ofMsg ))
    | NewPage (page,delay) ->
        let msg =
            Elmish.Navigation.Navigation.newUrl(Global.toHash(page))
        model, msg
        
