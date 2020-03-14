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

type Testcase =
    {
        input : option<DeleteInfo> * NamePair
        ExpectedResult : Data.InstructionData * option<seq<modificationInfo>>
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
    }

[<Fact>]
let ``TestModificationsLogic`` () =
        
    let getTestModInfo   names
                       ( newDelOrReg : seq<DeleteInfo option> ) =
        Seq.zip names newDelOrReg
        |> Seq.map (fun (num,newDelOrReg) ->
            (num |> string |> fun x -> "part" + x) ,
              newDelOrReg)
        |> Seq.map (fun (title,newDelOrReg) ->
            let currName = Some(title)
            getModInfo   currName
                         newDelOrReg
        )
                

    let getInstructionSet ( nums : seq<int> ) =
        {
            Data.InstructionData.Title = "Instruction"
            Data.InstructionData.Data =
                nums
                |> Seq.map (fun num -> num |> string)
                |> Seq.map (fun title ->
                    getPart
                        (("part" + title) |> Some )
                         None
                         None)
        }

    let namePairFunc currName newName =
        {
            CurrName = currName
            NewName = None
        }

    let repeatOfSame obj amount =
        [1..amount]
        |> Seq.map (fun _ -> obj)

    let testCaseDelOrRegChanged =
        seq[
            "Delete" |> (Delete >> Some)
            "Delete" |> (Delete >> Some)
            "Delete" |> (Delete >> Some)
            "Delete" |> (Delete >> Some)
            "Regret" |> (Regret >> Some)
        ]

    let caseOneNewModInfo =
        seq[
             ( "part1",
               Some "part2",
               None)

             ( "part5",
               Some "part2",
               None)

             ( "part2",
               None,
               Some(Delete("Delete")))
        ]



    let caseOneInstructionsinput =
        seq[
            getInstructionSet(
                seq[1;2;3;4;5]
            )
            getInstructionSet(
              seq[2;1;3;4;5]  
            )
            getInstructionSet(
              seq[5;1;3;4;2]
            )
        ]

    let caseOneInstructionsResult =
        seq[
            getInstructionSet(
              seq[2;1;3;4;5]  
            )
            getInstructionSet(
              seq[5;1;3;4;2]  
            )
            getInstructionSet(
              seq[5;1;3;4;2]  
            ) 
        ]

    let caseOneModinfoInputs =
        let subCase1 =
            getTestModInfo (seq[1;2;3;4;5])
                           (repeatOfSame ("Delete" |> Delete |> Some) 5)
        let subCase2 =
            getTestModInfo (seq[2;1;3;4;5])
                           (repeatOfSame ("Delete" |> Delete |> Some) 5)
        let subCase3 =
            getTestModInfo (seq[5;1;3;4;2])
                           (repeatOfSame ("Delete" |> Delete |> Some) 5)
        seq[
             subCase1
             subCase2
             subCase3
        ]

    let caseOneModInfoResult =
        let subCase1 =
            getTestModInfo (seq[2;1;3;4;5])
                            (repeatOfSame ("Delete" |> (Delete >> Some)) 5)
        let subCase2 =
            getTestModInfo (seq[5;1;3;4;2])          
                            (repeatOfSame ("Delete" |> (Delete >> Some)) 5)
        let subCase3 =
            getTestModInfo (seq[5;1;3;4;2])
                            testCaseDelOrRegChanged
        seq[
            subCase1
            subCase2
            subCase3
        ]
    let testCases =
        [0..caseOneNewModInfo |> Seq.length |> fun x -> x - 1]
        |> Seq.map (fun pos ->
            {|
            NewInput = caseOneNewModInfo |> Seq.item pos
            InstructionInput = caseOneInstructionsinput |> Seq.item pos
            InstructionResult = caseOneInstructionsResult |> Seq.item pos
            currModInfoInput = caseOneModinfoInputs |> Seq.item pos
            CurrModInfoResult = caseOneModInfoResult |> Seq.item pos

             |})

    testCases
    |> Seq.iter (fun case ->
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

        [0..instructionResultActual.Data |> Seq.length |> fun x -> x - 1]
        |> Seq.iter (fun pos ->
            let getTitle ( sequence : seq<Data.partData> ) position =
                sequence
                |> Seq.item position
                |> fun x -> x.Title

            let extractModInfoData ( modInfo : seq<modificationInfo> ) = 
                modInfo
                |> Seq.map (fun infoVal ->
                        let delOrRegStr =
                            match infoVal.DelOrReg with
                            | Some delOrRegVal ->
                                match delOrRegVal with
                                | Instruction.Types.Regret str -> str
                                | Instruction.Types.Delete str -> str
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

                Seq.zip infos infosCompare
                |> Seq.forall (fun (info,infoComp) ->
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
                
                
                
                

