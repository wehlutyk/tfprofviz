module Viz exposing (profileViz)

import Hex
import Html
import Murmur3
import String
import Svg exposing (circle, svg, text, text_)
import Svg.Attributes exposing (..)
import Tuple
import Types exposing (Profile, Timing, Tree(..), Unit)


profileViz : Profile -> Html.Html msg
profileViz profile =
    Html.div []
        [ Html.text "Information we have:"
        , profile.fieldNames
            |> Types.toList
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
    5


timeToLims : ( Float, Float ) -> Float -> Float -> Float
timeToLims ( startLim, endLim ) maxTime time =
    let
        range =
            endLim - startLim

        prop =
            time / maxTime
    in
    startLim + prop * range


cumsumHelp : Float -> List Float -> List Float
cumsumHelp a list =
    case list of
        head :: tail ->
            (a + head) :: head :: tail

        [] ->
            [ a ]


cumsum : List Float -> List Float
cumsum list =
    List.foldl cumsumHelp [ 0 ] list


listToPairs : List a -> List ( a, a )
listToPairs list =
    case list of
        [] ->
            []

        head :: [] ->
            []

        _ ->
            List.map2 Tuple.pair (List.drop 1 list) list


treeViz depth lims (Tree nodeNames nodeTimings children) =
    let
        totTime =
            nodeTimings
                |> Types.first
                |> Tuple.second
                |> Types.toFloat

        nodeChildTime =
            nodeTimings
                |> Types.first
                |> Tuple.first
                |> Types.toFloat

        nodeChildLims =
            ( Tuple.first lims
            , timeToLims lims totTime nodeChildTime
            )

        childrenLims =
            children
                |> List.map (\(Tree _ childTimings _) -> childTimings)
                |> List.map Types.first
                |> List.map Tuple.second
                |> List.map Types.toFloat
                |> cumsum
                |> List.map (\t -> timeToLims lims totTime (t + nodeChildTime))
                |> listToPairs

        nodeColor =
            nodeNamesToHexColor nodeNames

        nodeArc =
            arc depth lims nodeColor

        nodeChildArc =
            arc (depth + 1) nodeChildLims nodeColor

        childrenVizs =
            List.map2 (treeViz (depth + 1)) childrenLims children
                |> List.concat
    in
    nodeArc :: nodeChildArc :: childrenVizs


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
