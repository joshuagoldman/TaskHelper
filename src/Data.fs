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
                                Title = "Example Part 1"
                                InstructionVideo = "ExampleVideo1.mp4"
                                InstructionTxt = ""
                            }

                            {
                                Title = "Example Part 2"
                                InstructionVideo = "ExampleVideo2.mp4"
                                InstructionTxt = "First example video"
                            }

                            {
                                Title = "Example Part 3"
                                InstructionVideo = "ExampleVideo3.mp4"
                                InstructionTxt = "Second example video"
                            }

                            {
                                Title = "Example Part 4"
                                InstructionVideo = "ExampleVideo4.mp4"
                                InstructionTxt = "Third example video"
                            }

                            {
                                Title = "Example Part 5"
                                InstructionVideo = "ExampleVideo5.mp4"
                                InstructionTxt = "Fourth example video"
                            }
                        ]
            }
        ]

