module App.State

open Controls
open Elmish
open Feliz
open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Browser
open Global
open Types
open Browser

let urlUpdate (result : Page option) model =
    match result with
    | None ->
        console.error("Error parsing url")
        model, Navigation.modifyUrl (toHash model.CurrentPage)
    | Some page ->
        { model with CurrentPage = page }, []

let init result =

      let (user, userCmd) = User.State.init()
      let (model, cmd) =
        urlUpdate result
            {
                CurrentPage = Login
                Login = Login.State.init() |> fun (a,_) -> a
                User = user
            }

      model, Cmd.batch [ cmd
                         Cmd.map UserMsg userCmd ]

let pageParser: Parser<Page->Page,Page> =
    oneOf [
        map Login (s "login")
        User(Part) |> fun a -> map a  (s "part")
        User(InstructionSearch) |> fun a -> map a  (s "search")
        User(Instruction) |> fun a -> map a  (s "instruction")
    ]

let update msg model : Model * Cmd<App.Types.Msg> =
    match msg with
    | UserMsg msg ->
        let (userModel, userModelCmd) = User.State.update msg model.User
        { model with User = userModel }, Cmd.map UserMsg userModelCmd

