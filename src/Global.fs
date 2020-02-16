module Global

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

