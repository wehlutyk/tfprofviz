module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput)
import Parser
import ProfileParser exposing (profile)
import Types exposing (Profile, Tree)
import Viz exposing (profileViz)


main =
    Browser.sandbox { init = initModel, update = update, view = view }



-- MODEL


type alias Model e =
    { input : String
    , parse : Result (List e) Profile
    }


initModel =
    { input = profileString
    , parse = Parser.run profile profileString
    }



-- MSG


type Msg
    = Input String



-- UPDATE


update msg model =
    case msg of
        Input string ->
            { model | input = string, parse = Parser.run profile string }


view model =
    let
        viz =
            case model.parse of
                Err e ->
                    Html.div [] []

                Ok parsedProfile ->
                    profileViz parsedProfile
    in
    div []
        [ Html.h2 [] [ text "Input" ]
        , Html.textarea [ onInput Input, style "width" "100%", style "height" "20em" ] [ text profileString ]
        , Html.h2 [] [ text "Viz" ]
        , viz
        , Html.h2 [] [ text "Model" ]
        , Html.p [] [ text (Debug.toString model.parse) ]
        ]



-- DATA


profileString =
    """
Profile:
node name | total execution time | accelerator execution time | cpu execution time
_TFProfRoot (--/9.66ms, --/1.21ms, --/8.43ms)
  optimizer (0us/6.79ms, 0us/886us, 0us/5.89ms)
  optimizer/gradients (0us/2.91ms, 0us/558us, 0us/2.35ms)
"""
