module User.Types

open Global

type Msg =
    | PartMsg of InstructionSearch.State.Msg
    | InstructionSearchMsg of InstructionSearch.State.Msg
    | InstructionMsg of InstructionSearch.State.Msg

type Model =
    {
      CurrentPage: Global.UserPage
      InstructionSearch: InstructionSearch.Types.Model
      UserName : string
      Password : string
    }
