module Colors exposing (defaultColors)

import Css exposing (..)


type alias Colors =
    { textHighlight : Css.Color
    , textNormal : Css.Color
    , selectedBackground : Css.Color
    , normalBackground : Css.Color
    , confirmButton : Css.Color
    , normalButton : Css.Color
    , warning : Css.Color
    , success : Css.Color
    , fail : Css.Color
    }


defaultColors =
    Colors
        (Css.rgb 255 149 0)
        (Css.rgb 0 0 0)
        (Css.rgb 255 204 0)
        (Css.rgb 255 255 255)
        (Css.rgb 76 217 100)
        (Css.rgb 0 122 255)
        (Css.rgb 255 204 0)
        (Css.rgb 76 217 100)
        (Css.rgb 255 45 85)


solarizedColors =
    Colors
        (Css.hex "b58900")
        (Css.hex "657b83")
        (Css.hex "fdf6e3")
        (Css.hex "ffffff")
        (Css.hex "859900")
        (Css.hex "eee8d5")
        (Css.hex "b58900")
        (Css.hex "859900")
        (Css.hex "dc322f")



{-
   solarizedMainColor=#002b36
   solarized_base03=#002b36
   solarized_base02=#073642
   solarized_base01=#586e75
   solarized_base00=#657b83
   solarized_base0=#839496
   solarized_base1=#93a1a1
   solarized_base2=#eee8d5
   solarized_base3=#fdf6e3
   solarized_yellow=#b58900
   solarized_orange=#cb4b16
   solarized_red=#dc322f
   solarized_magenta=#d33682
   solarized_violet=#6c71c4
   solarized_blue=#268bd2
   solarized_cyan=#2aa198
   solarized_green=#859900
-}
