module Tests

open System
open Xunit
open Part
open Part.State
open Part.Types
open Part.Logic

[<Fact>]
let ``go2PreviousOrNextTest`` () =
    let testModel =
        Part.State.init() |> fun (model, _) -> model

    let newPartResultNextButton = [ 0 ; 2 ; 3 ; 4 ; 5 ; 6 ; 7 ; 9]

    let newPartResult =  newPartResultNextButton
                         |> Seq.map (fun num -> num + 10)
                         |> Seq.append newPartResultNextButton

    let nextButtonVisibleResult =  [ 0 ; 2 ; 4 ; 5 ; 7 ; 9 ]
    let nextButtonInVisibleResult = [ 1 ; 3 ; 6 ; 8 ]

    let PreviousButtonVisibleResult = nextButtonVisibleResult
                                      |> Seq.map (fun num -> num + 10)
    let PreviousButtonInVisibleResult = nextButtonInVisibleResult
                                        |> Seq.map (fun num -> num + 10)
    

    let testing model buttonName pos =
        let testDispatch msg =
            match msg with
            | NewPart2Show _ -> newPartResult
                                |> Seq.exists (fun num -> num = pos)
                                |> fun result -> Assert.True (result)
            | MakeButtonVisible (isVisible,buttonChoice) ->
                match buttonChoice with
                | "NextButton" ->
                    match isVisible with
                    | true -> nextButtonVisibleResult
                              |> Seq.exists (fun num -> num = pos)
                              |> fun result -> Assert.True (result)
                    | false -> nextButtonInVisibleResult
                               |> Seq.exists (fun num -> num = pos)
                               |> fun result -> Assert.True (result)
                | "PreviousButton" ->
                    match isVisible with
                    | true -> PreviousButtonVisibleResult
                              |> Seq.exists (fun num -> num = pos)
                              |> fun result -> Assert.True (result)
                    | false -> PreviousButtonInVisibleResult
                               |> Seq.exists (fun num -> num = pos)
                               |> fun result -> Assert.True (result)

                | _ -> Assert.True (false)
            |> fun _ -> Assert.True (false)

        go2PreviousOrNext model testDispatch buttonName

    let buttonNameCases =
        seq
            [
                "NextButton"
                "PreviousButton"
            ]


    let dataCases =
        Data.allData
        |> Seq.item 0
        |> fun x -> x.Data
        |> Seq.map (fun dt -> { testModel with Data = dt } )

    let allCases =
        dataCases
        |> Seq.collect (fun case -> buttonNameCases
                                    |> Seq.map (fun button ->
                                                        {|
                                                            Button = button
                                                            Model = case
                                                        |}))

    Seq.zip allCases [0..allCases |> Seq.length |> fun x -> x - 1]
    |> Seq.map (fun (case,pos) -> testing case.Model case.Button pos) 
