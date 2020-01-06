module NewAdd.Types

open Controls
open Part
open Elmish
open Types
open Data
open Browser

type Msg =
    | StoreNewPartMsg of Browser.Types.File * InstructionData
    | AddNewPartMsg of Browser.Types.File * InstructionData

type SearchResult =
    | Instruction of Data.InstructionData * Cmd<Instruction.State.Msg>
    | Part of Data.partData * Cmd<Part.State.Msg>

type Model =
    {
       NewAddDropDown : AppearanceAttributes
       VideoUpload : AppearanceAttributes
       NewAddButton : AppearanceAttributes
       AddText : AppearanceAttributes
    }

