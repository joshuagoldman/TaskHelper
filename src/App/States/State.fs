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
open Data
open Logic

let urlUpdate (result : Page option) model =
    match result with
    | None ->
        console.error("Error parsing url")
        model, Navigation.modifyUrl (toHash model.CurrentPage)
    | Some page ->
        match page with
        | Login ->
            model, []
        | User userPage ->
            let userPageOption = Some(userPage)
            let (user, _) = User.State.urlUpdate userPageOption model.User
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
        User(NewAdd) |> fun a -> map a (s "newAdd")
    ]

let matchValidity validityObject =
    match validityObject with
    | Valid str -> str
    | Invalid -> "Invalid"

let update msg model : Model * Cmd<App.Types.Msg> =
    match msg with
    | UserMsg msg ->
        let (userModel, userModelCmd) = User.State.update msg model.User
        let (newModel, newCmd) =
            { model with User = userModel }, Cmd.map UserMsg userModelCmd
        newModel, newCmd
    | LoginMsg msg ->
        let (login, loginCmd) = Login.State.update msg model.Login
        { model with Login = login }, Cmd.map LoginMsg loginCmd
    | LoginToUser AsyncOperationEvent.Started->
                 model,  Cmd.ofMsg (Logic.loginToUserIfSuccess model)
                   
    | LoginToUser (AsyncOperationEvent.Finished page) ->
            let (newModel, _) =
                    { model with CurrentPage = page }, []
            newModel, []

