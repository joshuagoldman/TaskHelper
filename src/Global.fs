module Global

type MainPage =
    | Part
    | Instruction
    | InstructionSearch

type Page =
    | Login
    | Main of MainPage

let toHashMain page =
    match page with
    | Part -> "#part"
    | Instruction -> "#instruction"
    | InstructionSearch -> "#search"

let toHash page =
    match page with
    | Login -> "#login"
    | Main mainPage -> toHashMain mainPage

