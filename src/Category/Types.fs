module Category.Types

open Controls
open Part
open Elmish

type PartData =
    {
        Control : AppearanceAttributes
        Data : Part.Types.Model * Cmd<Part.State.Msg>
    }

type Model =
    {
        Parts : seq<PartData>
        Title : string
    }

type Msg =
    | NewInstruction2Show of Model
