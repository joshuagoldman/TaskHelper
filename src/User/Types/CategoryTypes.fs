module Category.Types

open Controls
open Part
open Elmish

type Msg =
    | NewInstruction2Show of Data.InstructionData

type Model =
    {
        Blocks : seq<AppearanceAttributes>
    }
