module Main.Types

open Global

type Msg =
    | PartMsg of InstructionSearch.State.Msg
    | InstructionSearchMsg of InstructionSearch.State.Msg
    | InstructionMsg of InstructionSearch.State.Msg

type Model =
    {
      CurrentPage: Global.MainPage
      InstructionSearch: InstructionSearch.Types.Model
    }
