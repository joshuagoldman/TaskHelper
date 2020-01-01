module User.Types

open Global

type Msg =
    | InstructionMsg of Instruction.State.Msg
    | InstructionSearchMsg of InstructionSearch.Types.Msg
    | CategorySearchMsg of Category.Types.Msg

type Model =
    {
      Username : string
      Password : string
      CurrentPage: Global.UserPage
      InstructionSearch: InstructionSearch.Types.Model
      Category : Category.Types.Model
      UserData : seq<Instruction.Types.Model>
      Instruction: Instruction.Types.Model
    }
