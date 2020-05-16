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
open Instruction
open Feliz
open Elmish
open Browser
open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props

type Testcase =
    {
        input : option<DeleteInfo> * NamePair
        ExpectedResult : Data.InstructionData * option<array<modificationInfo>>
    }

type SavingChoiceArgs = {
    Instruction : Data.InstructionData
    ModInfo : array<modificationInfo> Option
    Instructions : array<Data.InstructionData>
    Result : User.Types.newSaveResult -> bool
}

let getPart ( title : option<string> )
            ( video : option<string> )
            ( instruction : option<string> )  =
    {
        Data.partData.Title = if title.IsSome then title.Value else ""
        Data.partData.InstructionVideo = if video.IsSome then video.Value else ""
        Data.partData.InstructionTxt = if instruction.IsSome then instruction.Value else ""
    }

type ChooseModInfo =
    | CurrName of string
    | NewName of string Option
    | DelOrReg of DeleteInfo

let getModInfo ( currNameOpt  : option<string>)
                 delOption  =
    let currName = if currNameOpt.IsSome
                   then currNameOpt.Value
                   else ""
    {
        DelOrReg = delOption
        Names =
            {
                CurrName = currName
                NewName = None
            }
        Status = [|StatusExisting(currName)|]
    }

let getTestModInfo   names
                   ( newDelOrReg : array<DeleteInfo option> ) =
    Array.zip names newDelOrReg
    |> Array.map (fun (num,newDelOrReg) ->
        (num |> string |> fun x -> "part" + x) ,
          newDelOrReg)
    |> Array.map (fun (title,newDelOrReg) ->
        let currName = Some(title)
        getModInfo   currName
                     newDelOrReg
    )
            
let getInstructionSet ( nums : array<int> ) =
    {
        Data.InstructionData.Title = "Instruction"
        Data.InstructionData.Data =
            nums
            |> Array.map (fun num -> num |> string)
            |> Array.map (fun title ->
                getPart
                    (
                        ("part" + title) |> Some )
                        (("Instruction" + title) |> Some)
                        (("Video" + title) |> Some)
                    )
    }


type InstructionObtainingStyle =
    | Simple of array<int>
    | NotSimple of array<int * option<string>>

let getInstructionSetNotSimple ( style : InstructionObtainingStyle ) =
        match style with
        | Simple sequence ->
            sequence |> getInstructionSet
        | NotSimple sequence ->
            {
                Data.InstructionData.Title = "Instruction"
                Data.InstructionData.Data =
                    
                    sequence
                    |> Array.map (fun (num ,str)-> (num |> string,str))
                    |> Array.map (fun (title,fileName) ->
                        let getNewFileName name =
                            if fileName.IsSome
                            then (name + fileName.Value) |> Some
                            else (name + title |> Some)
                        getPart
                            (
                                ("part" + title) |> Some )
                                (getNewFileName "Instruction")
                                (getNewFileName "Video"
                            ))
            }

let repeatOfSame obj amount =
    [|1..amount|]
    |> Array.map (fun _ -> obj)

[<Fact>]
let ``TestModificationsLogic`` () =
        
    let testCaseDelOrRegChanged =
        [|
            "Delete" |> (Delete >> Some)
            "Delete" |> (Delete >> Some)
            "Delete" |> (Delete >> Some)
            "Delete" |> (Delete >> Some)
            "Regret" |> (Regret >> Some)
        |]

    let caseOneNewModInfo =
        [|
             ( "part1",
               Some "part2",
               None)

             ( "part5",
               Some "part2",
               None)

             ( "part2",
               None,
               Some(Delete("Delete")))
        |]

    let caseOneInstructionsinput =
        [|
            getInstructionSet(
                [|1;2;3;4;5|]
            )
            getInstructionSet(
              [|2;1;3;4;5|]  
            )
            getInstructionSet(
              [|5;1;3;4;2|]
            )
        |]

    let caseOneInstructionsResult =
        [|
            getInstructionSet(
              [|2;1;3;4;5|]  
            )
            getInstructionSet(
              [|5;1;3;4;2|]  
            )
            getInstructionSet(
              [|5;1;3;4;2|]  
            ) 
        |]

    let caseOneModinfoInputs =
        let subCase1 =
            getTestModInfo ([|1;2;3;4;5|])
                           (repeatOfSame ("Delete" |> Delete |> Some) 5)
        let subCase2 =
            getTestModInfo ([|2;1;3;4;5|])
                           (repeatOfSame ("Delete" |> Delete |> Some) 5)
        let subCase3 =
            getTestModInfo ([|5;1;3;4;2|])
                           (repeatOfSame ("Delete" |> Delete |> Some) 5)
        [|
             subCase1
             subCase2
             subCase3
        |]

    let caseOneModInfoResult =
        let subCase1 =
            getTestModInfo ([|2;1;3;4;5|])
                            (repeatOfSame ("Delete" |> (Delete >> Some)) 5)
        let subCase2 =
            getTestModInfo ([|5;1;3;4;2|])          
                            (repeatOfSame ("Delete" |> (Delete >> Some)) 5)
        let subCase3 =
            getTestModInfo ([|5;1;3;4;2|])
                            testCaseDelOrRegChanged
        [|
            subCase1
            subCase2
            subCase3
        |]
    let testCases =
        [|0..caseOneNewModInfo |> Array.length |> fun x -> x - 1|]
        |> Array.map (fun pos ->
            {|
            NewInput = caseOneNewModInfo |> Array.item pos
            InstructionInput = caseOneInstructionsinput |> Array.item pos
            InstructionResult = caseOneInstructionsResult |> Array.item pos
            currModInfoInput = caseOneModinfoInputs |> Array.item pos
            CurrModInfoResult = caseOneModInfoResult |> Array.item pos

             |})

    testCases
    |> Array.iter (fun case ->
        let (currName,newName,dOrR) =
            case.NewInput
            |> fun (currName,newName,dOrR) ->
                currName,newName,dOrR
        let (instructionResultActual, modInfoOResultActual) =
            Instruction.Logic.updateCurrPositionsTestable
                                                   case.InstructionInput
                                                   case.currModInfoInput
                                                   dOrR
                                                   currName
                                                   newName

        [|0..instructionResultActual.Data |> Array.length |> fun x -> x - 1|]
        |> Array.iter (fun pos ->
            let getTitle ( sequence : array<Data.partData> ) position =
                sequence
                |> Array.item position
                |> fun x -> x.Title

            let extractModInfoData ( modInfo : array<modificationInfo> ) = 
                modInfo
                |> Array.map (fun infoVal ->
                        let delOrRegStr =
                            match infoVal.DelOrReg with
                            | Some delOrRegVal ->
                                match delOrRegVal with
                                | Instruction.Types.Regret str -> str
                                | Instruction.Types.DeleteInfo.Delete str -> str
                            | None -> ""
                        let newName =
                            match infoVal.Names.NewName with
                            | Some n -> n
                            | None -> ""
                        infoVal.Names.CurrName,
                        newName,
                        delOrRegStr
                        )
            let compareModInfos modInfo newModinfo =
                let infos = extractModInfoData modInfo
                let infosCompare = extractModInfoData newModinfo

                Array.zip infos infosCompare
                |> Array.forall (fun (info,infoComp) ->
                    let (a1,b1,c1) = info |> fun (x,y,z) -> x,y,z
                    let (a2,b2,c2) = infoComp |> fun (x,y,z) -> x,y,z

                    a1 =a2 && b1 = b2 && c1 = c2
                    )

                
            let instrResult =
                getTitle instructionResultActual.Data pos =
                    getTitle case.InstructionResult.Data pos
            let modInfResult =
                compareModInfos modInfoOResultActual case.CurrModInfoResult 
            Assert.True(
                instrResult && modInfResult
            ) 
            ))

[<Fact>]
let ``TestSavingChoices`` () =
    let existingInstructions =
        [|
            [|1;2;3;4;5|]
            |> getInstructionSet
            |> fun instr ->
                { instr with Title = "Instruction1"}

            [|1;2;3;4;5|]
            |> getInstructionSet
            |> fun instr ->
                { instr with Title = "Instruction2"}
        |]

    let modInfoCases =
        [|
            getTestModInfo ([|1;2;3;4;5|])
                           (repeatOfSame ("Delete" |> Delete |> Some) 5)
            |> Some

            getTestModInfo ([|1;2;3;4;5|])
                           (
                                [|
                                    ("Delete" |> Delete |> Some)
                                    ("Delete" |> Delete |> Some)
                                    ("Delete" |> Delete |> Some)
                                    ("Delete" |> Delete |> Some)
                                    ("Regret" |> Regret |> Some)
                                |]
                           ) |> Some

            getTestModInfo ([|1;2;3;4;5|])
                           (
                                [|
                                    ("Regret" |> Regret |> Some)
                                    ("Regret" |> Regret |> Some)
                                    ("Regret" |> Regret |> Some)
                                    ("Regret" |> Regret |> Some)
                                    ("Regret" |> Regret |> Some)
                                |]
                           ) |> Some

            getTestModInfo ([|1;2;2;4;5;6|])
                           (repeatOfSame ("Delete" |> Delete |> Some) 5)
            |> Some

            getTestModInfo ([|1;2;3;4;5;6|])
                           (repeatOfSame ("Delete" |> Delete |> Some) 6)
            |> Some

            getTestModInfo ([|1;0;3;4;5|])
                           (repeatOfSame ("Delete" |> Delete |> Some) 5)
            |> Some

            getTestModInfo ([|1;0;3;4;5;6|])
                           (repeatOfSame ("Delete" |> Delete |> Some) 6)
            |> Some

            getTestModInfo ([|1;0;3;4;5;6|])
                           (
                                [|
                                    ("Regret" |> Regret |> Some)
                                    ("Delete" |> Delete |> Some)
                                    ("Delete" |> Delete |> Some)
                                    ("Delete" |> Delete |> Some)
                                    ("Delete" |> Delete |> Some)
                                    ("Delete" |> Delete |> Some)
                                |]
                           ) |> Some
            getTestModInfo ([|1;2;3;4;5;6|])
                        (
                             [|
                                 ("Regret" |> Regret |> Some)
                                 ("Delete" |> Delete |> Some)
                                 ("Delete" |> Delete |> Some)
                                 ("Delete" |> Delete |> Some)
                                 ("Regret" |> Regret |> Some)
                                 ("Delete" |> Delete |> Some)
                             |]
                        ) |> Some
            getTestModInfo ([|1;2;6;4;5|])
                        (
                             [|
                                 ("Delete" |> Delete |> Some)
                                 ("Delete" |> Delete |> Some)
                                 ("Delete" |> Delete |> Some)
                                 ("Delete" |> Delete |> Some)
                                 ("Regret" |> Regret |> Some)
                             |]
                        ) |> Some
        |]

    let instructionCases =
        [|
            [|1;2;3;4;5|]
            |> getInstructionSet
            |> fun instr ->
                { instr with Title = "Instruction1"}

            [|(1,None);(2,None);(3,None);(4,None);(5,Some("5"))|]
            |> NotSimple
            |> getInstructionSetNotSimple
            |> fun instr ->
                { instr with Title = "Instruction2"}

            [|1;2;3;4;5|]
            |> getInstructionSet
            |> fun instr ->
                { instr with Title = "Instruction1"}

            [|1;2;2;4;5|]
            |> getInstructionSet
            |> fun instr ->
                { instr with Title = "Instruction1"}

            [|1;2;3;4;5;6|]
            |> getInstructionSet
            |> fun instr ->
                { instr with Title = "Instruction1"}

            [|(1,None);(0,Some("2"));(3,None);(4,None);(5,None)|]
            |> NotSimple
            |> getInstructionSetNotSimple
            |> fun instr ->
                { instr with Title = "Instruction2"}

            [|(1,None);(0,Some("2"));(3,None);(4,None);(5,None);(6,None)|]
            |> NotSimple
            |> getInstructionSetNotSimple
            |> fun instr ->
                { instr with Title = "Instruction2"}

            [|(1,None);(0,Some("2"));(3,None);(4,None);(5,None);(6,None)|]
            |> NotSimple
            |> getInstructionSetNotSimple
            |> fun instr ->
                { instr with Title = "Instruction2"}

            [|1;2;3;4;5;6|]
            |> getInstructionSet
            |> fun instr ->
                { instr with Title = "Instruction1"}

            [|(1,None);(2,None);(6,Some("3"));(4,None);(5,None)|]
            |> NotSimple
            |> getInstructionSetNotSimple
            |> fun instr ->
                { instr with Title = "Instruction1"}
        |]
    let first arg =
        match arg with
        | User.Types.newSaveResult.ThatInstructionAlreadyExists _ -> true
        | _ -> false
    let second arg =
        match arg with
        | User.Types.newSaveResult.SaveExistingPartsToDelete _ -> true
        | _ -> false
    let third arg =
        match arg with
        | User.Types.newSaveResult.InstructionIsDelete _ -> true
        | _ -> false
    let fourth arg =
        match arg with
        | User.Types.newSaveResult.InstructionHasNotDistinctTitles _ -> true
        | _ -> false

    let fifth arg =
        match arg with
        | User.Types.newSaveResult.SaveExisitngNewFIles _ -> true
        | _ -> false

    let sixth arg =
        match arg with
        | User.Types.newSaveResult.SaveExistingNewTitles _ -> true
        | _ -> false

    let seventh arg =
        match arg with
        | User.Types.newSaveResult.SaveExistingNewFilesAndTItles _ -> true
        | _ -> false
    let eighth arg =
        match arg with
        | User.Types.newSaveResult.SaveExistingNewFilesAndTItlesPartsToDelete _ -> true
        | _ -> false
    let ninth arg =
        match arg with
        | User.Types.newSaveResult.SaveExistingNewFilesPartsToDelete _ -> true
        | _ -> false
    let tenth arg =
        match arg with
        | User.Types.newSaveResult.SaveExistingNewTItlesPartsToDelete _ -> true
        | _ -> false

    let resultCases =
        [|
            first
            second
            third
            fourth
            fifth
            sixth
            seventh
            eighth
            ninth
            tenth
        |]

    let cases =
        Array.zip modInfoCases [|0..modInfoCases |> Array.length |> fun x -> x - 1|]
        |> Array.map (fun (modInfo,pos) ->
            {
                Instruction = instructionCases |> Array.item pos
                ModInfo = modInfo
                Instructions = existingInstructions
                Result = resultCases |> Array.item pos
            })

    cases
    |> Array.iter (fun case ->
        let resultActual =
            User.Logic.savingChoicesTestable
                        case.Instruction
                        case.ModInfo
                        case.Instructions
        Assert.True(resultActual |> case.Result))


                
                

