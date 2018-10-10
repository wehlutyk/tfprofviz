module Viz exposing (profileViz)

import Html
import Svg exposing (..)
import Types exposing (Profile, Timing, Tree, Unit)


profileViz : Profile -> Html.Html msg
profileViz profile =
    Html.div [] []
