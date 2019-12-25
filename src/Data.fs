module Data

type partData =
    {
        InstructionVideo : string
        InstructionTxt : string
        Title : string
    }

type InstructionData =
    {
        Data : seq<partData>
        Title : string
    }

let allData =
    seq
        [
            {
                Title = "Example Instruction"
                Data =
                    seq
                        [
                            {
                                Title = ""
                                InstructionVideo = ""
                                InstructionTxt = ""
                            }

                            {
                                Title = ""
                                InstructionVideo = ""
                                InstructionTxt = ""
                            }

                            {
                                Title = ""
                                InstructionVideo = ""
                                InstructionTxt = ""
                            }
                        ]
            }
        ]

