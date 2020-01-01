module Global

type UserPage =
    | Part
    | Instruction
    | InstructionSearch
    | Category

type Page =
    | Login
    | User of UserPage 

let toHashUser page =
    match page with
    | Part -> "#part"
    | Instruction -> "#instruction"
    | InstructionSearch -> "#search"
    | Category -> "#category"

let toHash page =
    match page with
    | Login -> "#login"
    | User userPage -> toHashUser userPage

