module Global

type UserPage =
    | Part
    | Instruction
    | InstructionSearch

type Page =
    | Login
    | User of UserPage

let toHashUser page =
    match page with
    | Part -> "#part"
    | Instruction -> "#instruction"
    | InstructionSearch -> "#search"

let toHash page =
    match page with
    | Login -> "#login"
    | User userPage -> toHashUser userPage

