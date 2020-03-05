module Tests

open System
open Xunit
open Part
open Part.State
open Part.Types
open Part.Logic
open Thoth.Json
open FSharp.Data
open NewAdd.Types
open Instruction.Types

type Example =
    {
        test : string
    }

[<Fact>]
let ``TestModificationsLogic`` () =
        let modifiedInfoSeq =
            seq [
                {
                    CurrName = ""
                    NewName = ""
                }
                |> fun x -> (Some 0,Some true,x)

                {
                    CurrName = ""
                    NewName = ""
                }
                |> fun x -> (Some 0,Some true,x)

                {
                    CurrName = ""
                    NewName = ""
                }
                |> fun x -> (Some 0,Some true,x)
            ]

        let updateCurrPositions (
                                    currInstruction : Data.InstructionData,
                                    currModInfo : seq<modificationInfo>,
                                    delOrReg : DeleteInfo Option,
                                    namePair : NamePair
                                ) =
                
            
            ()
            |> function
                | _ when delOrReg.IsSome && && namePair.NewName.IsNone ->
                    currModInfo
                    |> Seq.map (fun info ->
                        info.Names.CurrName
                        |> function
                            | name when name = namePair.CurrName ->
                                info.DelOrReg
                                |> function
                                    | res when res.IsSome ->
                                        let newVal =
                                            match res.Value with
                                            | Delete _ ->
                                                Regret("Regret")
                                            | Regret _ ->
                                                Delete("Delete")
                                        { info with DelOrReg = Some newVal }
                                    | _ -> info
                            | _ -> info
                        )
                    |> Seq.filter (fun info ->
                        match info.DelOrReg.Value with
                        | Delete _ ->
                            true
                        | Regret _ ->
                            false)
                    |> fun newModInfo ->
                        newModInfo
                        |> Seq.map (fun info ->
                            currInstruction.Data
                            |> Seq.tryFind (fun part ->
                                part.Title = info.Names.CurrName)
                            |> function
                                | res when res.IsSome ->
                                    Some res.Value
                                | _ -> None)
                        |> Seq.choose id
                        |> fun parts ->
                            { CurrInstruction with Data = parts}, Some newModInfo
                | _ when namePair.NewName.IsSome && checkedOpt.IsNone ->
                    currModInfo
                    |> Seq.map (fun info ->
                        ()
                        |> function
                            | _ when info.Names.CurrName = namePair.CurrName ->
                                { info with Names = {
                                                CurrName = namePair.NewName.Value
                                                NewName = None
                                            }}
                            | _ when info.Names.CurrName = namePair.NewName.Value ->
                                { info with Names = {
                                                CurrName = namePair.CurrName
                                                NewName = None
                                            }}
                            | _ -> info
                        )
                    
                    |> Seq.map (fun info ->
                        currInstruction.Data
                        |> Seq.tryFind (fun part ->
                            part.Title = info.Names.CurrName)
                        |> function
                            | res when res.IsSome ->
                                Some res.Value
                            | _ -> None)
                    |> Seq.choose id
                    |> fun parts ->
                        { CurrInstruction with Data = parts}, None
                | _ -> currInstruction, None

