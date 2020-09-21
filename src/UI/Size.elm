module UI.Size exposing (width,gap)

import Html exposing (Attribute)
import Html.Attributes exposing (class)



--width from 1 to 12


width : Int -> Attribute msg
width num =
    class ("is-" ++ String.fromInt num)



--gap from 1 to 8


gap : Int -> Attribute msg
gap num =
    class ("is-" ++ String.fromInt num)
