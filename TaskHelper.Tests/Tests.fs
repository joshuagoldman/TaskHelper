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
               ( newNameOpt : option<string> )
                 delOption  =
    let currName = if currNameOpt.IsSome
                   then currNameOpt.Value
                   else ""
    let newName = if newNameOpt.IsSome
                  then Some newNameOpt.Value
                  else None
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
        
    let getTestModInfo names
                       ( newNames : seq<int option> )
                       ( newDelOrReg : seq<DeleteInfo option> ) =
        Seq.zip3 names newNames newDelOrReg
        |> Seq.map (fun (num,newNameNumOpt,newDelOrReg) ->
            let newName =
                if newNameNumOpt.IsSome
                then newNameNumOpt.Value
                     |> string
                     |> fun x -> ("part" + x)
                     |> Some 
                else None
            (num |> string |> fun x -> "part" + x) ,
              newName,
              newDelOrReg)
        |> Seq.map (fun (title,newTitle,newDelOrReg) ->
            let currName = Some(title)
            getModInfo   currName
                         newTitle
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
            "Regret" |> (Regret >> Some)
            "Delete" |> (Delete >> Some)
            "Delete" |> (Delete >> Some)
            "Delete" |> (Delete >> Some)
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
                           (repeatOfSame None 5)
                           (repeatOfSame ("Delete" |> Delete |> Some) 5)
        let subCase2 =
            getTestModInfo (seq[2;1;3;4;5])
                           (repeatOfSame None 5)
                           (repeatOfSame ("Delete" |> Delete |> Some) 5)
        let subCase3 =
            getTestModInfo (seq[5;1;3;4;2])
                           (repeatOfSame None 5)
                           (repeatOfSame ("Delete" |> Delete |> Some) 5)
        seq[
             subCase1
             subCase2
             subCase3
        ]

    let caseOneModInfoResult =
        let subCase1 =
            getTestModInfo (seq[2;1;3;4;5])
                            (repeatOfSame None 5)
                            (repeatOfSame ("Delete" |> (Delete >> Some)) 5)
        let subCase2 =
            getTestModInfo (seq[5;1;3;4;2])
                            (repeatOfSame None 5)            
                            (repeatOfSame ("Delete" |> (Delete >> Some)) 5)
        let subCase3 =
            getTestModInfo (seq[5;1;3;4;2])
                           ([5;1;3;4;2] |> Seq.map (fun i -> i |> Some))
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

    let ajustAfterNewModinfo ( currInstruction : Data.InstructionData )
                             ( newModinfo : seq<modificationInfo> ) :
                               ( Data.InstructionData * seq<modificationInfo> ) =
        newModinfo
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
            { currInstruction with Data = parts}, newModinfo

    let updateCurrPositions (currInstruction : Data.InstructionData)
                            ( currModInfo : seq<modificationInfo> )
                            ( delOrReg : DeleteInfo Option )
                              currName
                            ( newName : Option<string> ) :
                            Data.InstructionData *
                            seq<modificationInfo> =
        ()
        |> function
            | _ when delOrReg.IsSome && newName.IsNone ->
                currModInfo
                |> Seq.map (fun info ->
                    info.Names.CurrName
                    |> function
                        | name when name = currName ->
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
                |> fun newModInfo ->
                    newModInfo
                    |> ajustAfterNewModinfo currInstruction
            | _ when newName.IsSome && delOrReg.IsNone ->
                currModInfo
                |> Seq.map (fun info ->
                    ()
                    |> function
                        | _ when info.Names.CurrName = currName ->
                            { info with Names = {
                                            CurrName = newName.Value
                                            NewName = None
                                        }}
                        | _ when info.Names.CurrName = newName.Value ->
                            { info with Names = {
                                            CurrName = currName
                                            NewName = None
                                        }}
                        | _ -> info
                    )
                    
                |>  fun newModinfo ->
                    newModinfo
                    |> ajustAfterNewModinfo currInstruction
            | _ -> currInstruction, currModInfo

    testCases
    |> Seq.iter (fun case ->
        let (currName,newName,dOrR) =
            case.NewInput
            |> fun (currName,newName,dOrR) ->
                currName,newName,dOrR
        let (instructionResultActual, modInfoOResultActual) =
            updateCurrPositions case.InstructionInput
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
                
                
                
                

