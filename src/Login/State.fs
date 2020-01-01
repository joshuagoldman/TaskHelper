module Login.State

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

      let (main, mainCmd) = Main.State.init()
      let (model, cmd) =
        urlUpdate result
            {
                CurrentPage = Login
                Username = ""
                Password = ""
                Main = main
            }

      model, Cmd.batch [ cmd
                         Cmd.map MainMsg mainCmd ]

let rrr mainPage =
    match mainPage with
    | Part -> (s "part")
    | Instruction -> (s "instruction")
    | InstructionSearch -> (s "search")

let pageParser: Parser<Page->Page,Page> =
    oneOf [
        map Login (s "login")
        Main(Part) |> fun a -> map a  (s "part")
        Main(InstructionSearch) |> fun a -> map a  (s "part")
        Main(Instruction) |> fun a -> map a  (s "part")
    ]

let update msg model : Model * Cmd<Login.Types.Msg> =
    match msg with
    | LoginButtonClicked (usrName,passWord) ->
        { model with Username = usrName ; Password = passWord}, []
    | MainMsg msg ->
        let (mainModel, mainModelCmd) = Main.State.update msg model.Main
        { model with Main = mainModel }, Cmd.map MainMsg mainModelCmd

