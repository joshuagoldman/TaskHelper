module Global

open Feliz
open Fable.React

type UserPage =
    | Part
    | Instruction
    | InstructionSearch
    | NewAdd

type Page =
    | Login
    | User of UserPage 

let toHashUser page =
    match page with
    | Part -> "#part"
    | Instruction -> "#instruction"
    | InstructionSearch -> "#search"
    | NewAdd -> "#newAdd"

let toHash page =
    match page with
    | Login -> "#login"
    | User userPage -> toHashUser userPage

/// <summary>A simple div with custom properties.</summary>
///<c>infotext,style</c>
///<remarks>logindelay</remarks>
let divWithStyle ( className : Option<string> ) msg properties =
    Html.div[
        prop.className ( if className.IsSome then className.Value else "columnn" )
        properties
        prop.children[
            Html.br[]
            str msg
        ]
    ]

let getPositionSequence ( sequence : seq<'t> ) =
    [0..sequence |> Seq.length |> fun x -> x - 1]
    |> List.toSeq

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
        | _ when posOne = posTwo ||
                 len = 0 ->
            sequence
        | _ when [posOne;posTwo] 
                 |> Seq.exists(fun x -> x > len || x < 0) ->
            sequence
        | _ when  posOne = 0 && posTwo = len ||
                  posOne = len && posTwo = 0 ->
            let min = seq[posOne;posTwo] |> Seq.min
            let first = Arr.[len]
            let second = Arr.[1..len - 1]
            let third = Arr.[min]

            let newSeq =
                seq[third]
                |> Seq.append (second |> Array.toSeq)
                |> Seq.append (seq[first])
            newSeq

           
        | _ when posOne = 0 ||
                 posTwo = 0 ->
            let min = seq[posOne;posTwo] |> Seq.min
            let max = seq[posOne;posTwo] |> Seq.max
            let first = Arr.[max]
            let second = Arr.[1..max - 1]
            let third = Arr.[min]
            let fourth = Arr.[max + 1..len]

            let newSeq =
                (fourth |> Array.toSeq)
                |> Seq.append (seq[third])
                |> Seq.append (second |> Array.toSeq)
                |> Seq.append (seq[first]) 

            newSeq

        | _ when posOne = len ||
                 posTwo = len->
            let min = seq[posOne;posTwo] |> Seq.min
            let first = Arr.[0..min - 1]
            let second = Arr.[len]
            let third = Arr.[min + 1..len - 1]
            let fourth = Arr.[min]

            let newSeq =
                seq[fourth]
                |> Seq.append third
                |> Seq.append (seq[second])
                |> Seq.append first 

            newSeq

        | _ when posOne > 0 &&
                 posOne < len &&
                 posTwo > 0 &&
                 posTwo < len ->
                    let min = seq[posOne;posTwo] |> Seq.min
                    let max = seq[posOne;posTwo] |> Seq.max
                    let first = Arr.[0..min - 1]
                    let second = Arr.[max]
                    let third = Arr.[min + 1..max - 1]
                    let fourth = Arr.[min]
                    let fifth = Arr.[max + 1..len]

                    let newSeq =
                        fifth
                        |> Seq.append (seq[fourth])
                        |> Seq.append third
                        |> Seq.append (seq[second])
                        |> Seq.append first  

                    newSeq
        | _ ->
            sequence

