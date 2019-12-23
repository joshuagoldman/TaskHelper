module Controls

open Fable.React
open Fable.React.Props

type AppearanceAttributes =
    {
        Color : string
        Text_decoration : string
        Padding : string
        Font_Family : seq<string>
        Font_Size : string
        Position : string
        top : float
        left : float
        Background : string
        height : string
        width : string
        transition : string
        Display : string
        Opacity : string
        SelectItems : seq<string>
        Picture : string
        MuteMusic : bool
        Visible : string
        BackgroundMusic : string
        Text : string
    }

type MainMenuElements =
    {
        MainDivButton : AppearanceAttributes
        GenreCombobox : AppearanceAttributes
        MuteButton : AppearanceAttributes
    }

type ExercisePageElements =
    {
        ChooseHandAlternatives : AppearanceAttributes
        ChooseSongComboBox : AppearanceAttributes
        GoBackButton : AppearanceAttributes

    }
type Page =
    {
        MainMenu : MainMenuElements
        ExercisePage : ExercisePageElements
        Loader : AppearanceAttributes
    }


let defaultAppearanceAttributes = 
    {
        Color = "White" ;
        Text_decoration = "none" ;
        Padding = "5px 20px" ;
        Font_Family = seq[ "Roboto" ; "sans-serif"] ;
        Font_Size = "15px"
        Position = "" ;
        top = 10.0 ;
        left = 10.0 ;
        Background = "" ;
        height = "" ;
        width = "" ;
        transition = "" ;
        Display = "inline-block" ;
        Opacity = "1" ;
        SelectItems = [ "Select Music Genre" ; "Boogie Woogie" ; "Pop" ; "Classical"] ;
        Picture = ""
        MuteMusic = false
        Visible = "hidden"
        BackgroundMusic = ""
        Text = ""
    }

let initPages = { 
                    MainMenu = 
                        { MainDivButton = defaultAppearanceAttributes ;
                          GenreCombobox = defaultAppearanceAttributes ;
                          MuteButton = { defaultAppearanceAttributes 
                                         with Picture = "Pictures/MusicPicture.jpg" }
                                         }
                    ExercisePage =
                        { ChooseHandAlternatives = defaultAppearanceAttributes ;
                          ChooseSongComboBox = defaultAppearanceAttributes ;
                          GoBackButton = defaultAppearanceAttributes}

                    Loader = defaultAppearanceAttributes
                }

let configMainMenuButtonStyle ( opacity : string) = 
    [ 
        BorderRadius "50px"
        Height "20%"
        Padding("10em") 
        Width("fit-content")
        Padding "5px"
        Background "radial-gradient(circle at 0px 0px, #E4A91E 0%, #884E06 100%)" 
        Opacity opacity
        FontSize "25px"
        FontWeight "bold"
        BorderWidth "5px"
        BorderColor "black"
        Margin "1.5em 1em 0.5em 0.5em"
        FontFamily "Comic Sans MS, Comic Sans, cursive"
        Outline "none"
    ]

let configMuteButtonStyle ( opacity : string) =  
    [   BorderRadius "50px"
        Height "10%"
        Padding("10em") 
        Width("fit-content")
        Padding "5px"
        Background "radial-gradient(circle at 0px 0px, #E4A91E 0%, #884E06 100%)" 
        Opacity opacity
        FontSize "15px"
        FontWeight "bold"
        BorderWidth "5px"
        BorderColor "black"
        Margin "1.5em 1em 0.5em 0.5em"
        FontFamily "Comic Sans MS, Comic Sans, cursive"
        Outline "none"
    ]

