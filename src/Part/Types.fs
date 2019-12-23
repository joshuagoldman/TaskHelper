module Part.Types

open Controls

type Model =
    {
        NextButton : AppearanceAttributes
        PreviousButton : AppearanceAttributes
        GoBackButton : AppearanceAttributes
        Instruction : string
        Title : string
        Video : string option
        InstructionTxt : string option
    }
