module Part.Types

open Controls
open Data

type Msg =
    | NewPart2Show of Data.partData * Data.InstructionData
    | MakeButtonVisible of bool * string
    | SendErrorMessage of string
    | NewUserIdMsg of int

type Model =
    {
        NextButton : AppearanceAttributes
        PreviousButton : AppearanceAttributes
        Go2Instruction : AppearanceAttributes
        Data : Result<partData,string>
        Instruction : Result<InstructionData,string>
        UserId : Result<int,string>
    }
