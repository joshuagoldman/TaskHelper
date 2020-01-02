module App.State

open Controls
open Elmish
open Feliz
open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Browser
open Global
open App.Types
open Browser

let urlUpdate (result : Page option) model =
    match result with
    | None ->
        console.error("Error parsing url")
        model, Navigation.modifyUrl (toHash model.CurrentPage)
    | Some page ->
        match page with
        | Login -> { model with CurrentPage = Login }, []
        | User userPage ->
            let userPageOption = Some(userPage)
            let (user, userCmd) = User.State.urlUpdate userPageOption model.User
            { model with CurrentPage = result.Value ;
                         User = user}, []
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
        User(Category) |> fun a -> map a (s "category")
    ]

let update msg model : Model * Cmd<App.Types.Msg> =
    match msg with
    | UserMsg msg ->
        let (userModel, userModelCmd) = User.State.update msg model.User
        { model with User = userModel }, Cmd.map UserMsg userModelCmd
    | LoginMsg msg ->
        let (login, loginCmd) = Login.State.update msg model.Login
        { model with Login = login }, Cmd.map LoginMsg loginCmd

