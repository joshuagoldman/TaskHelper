module Global

type Page =
    | Part
    | Instruction
    | InstructionSearch

let toHash page =
    match page with
    | Part -> "#part"
    | Instruction -> "#instruction"
    | InstructionSearch -> "#instructionSearch"
