module Viz exposing (profileViz)

import Hex
import Html
import Murmur3
import String
import Svg exposing (circle, svg, text, text_)
import Svg.Attributes exposing (..)
import Types exposing (Profile, Timing, Tree(..), Unit)


profileViz : Profile -> Html.Html msg
profileViz profile =
    Html.div []
        [ Html.text "Information we have:"
        , Html.ul [] (List.map (\n -> Html.li [] [ Html.text n ]) profile.fieldNames)
        , svg
            [ width "100%"
            , height "300"
            , viewBox "0 0 100 100"
            ]
            (treeViz 0 profile.tree)
        ]


murmur3Seed =
    0


nodeNamesToHexColor nodeNames =
    Murmur3.hashString murmur3Seed (String.join "/" nodeNames)
        |> Hex.toString
        |> String.left 6
        |> String.padLeft 6 '0'
        |> String.cons '#'


arcWidth =
    3


treeViz depth (Tree nodeNames nodeTimings children) =
    let
        head =
            arc (arcWidth * (depth + 0.5)) 0.25 1 (nodeNamesToHexColor nodeNames)
    in
    head :: (List.concat <| List.map (\c -> treeViz (depth + 1) c) children)


toStyle listTuples =
    listTuples
        |> List.map (\( k, v ) -> k ++ ": " ++ v)
        |> String.join "; "


arc radius startProp stopProp color =
    arcDetails 50
        50
        radius
        (startProp * 2 * pi)
        (stopProp * 2 * pi)
        [ strokeWidth <| String.fromFloat arcWidth
        , fill "none"
        , style <| "stroke: " ++ color
        ]
        []


arcDetails x y radius startAngle stopAngle attrs contents =
    let
        perimeter =
            2 * pi * radius

        arcLength =
            (stopAngle - startAngle) * radius

        fx =
            String.fromFloat x

        fy =
            String.fromFloat y
    in
    circle
        ([ cx fx
         , cy fy
         , r <| String.fromFloat radius
         , strokeDasharray <| String.fromFloat perimeter
         , strokeDashoffset <| String.fromFloat (arcLength - perimeter)
         , transform <| "rotate(-" ++ String.fromFloat (startAngle * 180 / pi) ++ ", " ++ fx ++ ", " ++ fy ++ ")"
         ]
            ++ attrs
        )
        contents
