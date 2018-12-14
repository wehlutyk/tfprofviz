module Viz exposing (profileViz)

import Hex
import Html
import Murmur3
import String
import Svg exposing (circle, svg, text, text_)
import Svg.Attributes exposing (..)
import Tuple
import Types exposing (Profile, Timing, Tree(..), Unit, toList)


profileViz : Profile -> Html.Html msg
profileViz profile =
    Html.div []
        [ Html.text "Information we have:"
        , profile.fieldNames
            |> toList
            |> List.map (\n -> Html.li [] [ Html.text n ])
            |> Html.ul []
        , svg
            [ width "100%"
            , height "300"
            , viewBox "0 0 100 100"
            ]
            (treeViz 0 ( 0, 1 ) profile.tree)
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


treeViz depth lims (Tree nodeNames nodeTimings children) =
    let
        -- TODO:
        -- - get list of proportions (rhs gives total, lhs gives prop of parent in children (-- -> 0), and rhs of children gives props of children)
        -- - cumsum
        -- totTime =
        -- nodeChildLims
        -- childrenProps
        -- childrenLims
        nodeArc =
            arc depth lims (nodeNamesToHexColor nodeNames)

        childrenVizs =
            children
                |> List.map (treeViz (depth + 1) ( 0, 1 ))
                |> List.concat
    in
    nodeArc :: childrenVizs


toStyle listTuples =
    listTuples
        |> List.map (\( k, v ) -> k ++ ": " ++ v)
        |> String.join "; "


arc depth lims color =
    arcDetails 50
        50
        (arcWidth * (depth + 0.5))
        (Tuple.first lims * 2 * pi)
        (Tuple.second lims * 2 * pi)
        [ strokeWidth <| String.fromFloat arcWidth
        , fill "none"
        , style <| "stroke: " ++ color
        ]
        []


arcDetails x y radius startAngle endAngle attrs contents =
    let
        perimeter =
            2 * pi * radius

        arcLength =
            (endAngle - startAngle) * radius

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
