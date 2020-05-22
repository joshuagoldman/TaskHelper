module InstructionSearch.State

open Elmish
open Controls
open Types

let init() : InstructionSearch.Types.Model<User.Types.Msg> * Cmd<User.Types.Msg> =
  {
     SearchBar =  defaultAppearanceAttributes
     ResultFromSearch =
         [|
            Error ""
         |]
  }, []

let update msg model : Model<'a> * Cmd<User.Types.Msg> =
    match msg with
    | TextHasChanged str ->
        {model with SearchBar = {model.SearchBar with Text = str }}, []
    | ClearSearchResult -> { model with ResultFromSearch = [||] ;
                                        SearchBar =
                                            { model.SearchBar with Text = "" } }, []
    | GetNewInstruction instrInfoOpt ->
        { model with ResultFromSearch = instrInfoOpt }, []
            
