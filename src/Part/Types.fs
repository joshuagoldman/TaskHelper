module Part.Types

open Controls
open Data

type Model =
    {
        NextButton : AppearanceAttributes
        PreviousButton : AppearanceAttributes
        Go2Instruction : AppearanceAttributes
        Data : Result<partData,string>
        Instruction : Result<InstructionData,string>
    }
