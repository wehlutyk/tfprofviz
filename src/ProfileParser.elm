module ProfileParser exposing (profile)

import Parser exposing ((|.), (|=), Step(..), Trailing(..))
import Set
import Tuple
import Types exposing (Profile, Timing, Tree(..), Unit(..))


header : Parser.Parser ()
header =
    Parser.chompUntil "Profile:"
        |. Parser.symbol "Profile:"


isNoLineSpace : Char -> Bool
isNoLineSpace c =
    c == ' '


isVarChar : Char -> Bool
isVarChar c =
    Char.isAlphaNum c || c == '_' || c == '-'


noLineSpaces : Parser.Parser ()
noLineSpaces =
    Parser.chompWhile isNoLineSpace


noLineSpacesNonEmptyGet : Parser.Parser String
noLineSpacesNonEmptyGet =
    Parser.variable
        { start = isNoLineSpace
        , inner = isNoLineSpace
        , reserved = Set.empty
        }


fieldNames : Parser.Parser (List String)
fieldNames =
    Parser.sequence
        { start = ""
        , separator = "|"
        , end = ""
        , spaces = noLineSpaces
        , item = fieldName
        , trailing = Parser.Forbidden
        }


identifier : Parser.Parser String
identifier =
    Parser.variable
        { start = isVarChar
        , inner = isVarChar
        , reserved = Set.empty
        }


fieldName : Parser.Parser String
fieldName =
    listWithSeps identifier noLineSpacesNonEmptyGet
        |> Parser.map (String.join "")


listWithSeps : Parser.Parser x -> Parser.Parser x -> Parser.Parser (List x)
listWithSeps a sep =
    Parser.succeed (\head tail -> head :: List.reverse tail)
        |= a
        |= Parser.loop [] (listWithSepsHelp a sep)


listWithSepsHelp : Parser.Parser x -> Parser.Parser x -> List x -> Parser.Parser (Step (List x) (List x))
listWithSepsHelp a sep items =
    Parser.oneOf
        [ Parser.succeed (\sepItem item -> Loop (item :: sepItem :: items))
            |= Parser.backtrackable sep
            |= Parser.backtrackable a
            |. Parser.commit ()
        , Parser.succeed ()
            |> Parser.map (\_ -> Done items)
        ]


unit : Parser.Parser Unit
unit =
    Parser.oneOf
        [ Parser.map (\_ -> Millisecond) (Parser.symbol "ms")
        , Parser.map (\_ -> Microsecond) (Parser.symbol "us")
        ]


timing : Parser.Parser Timing
timing =
    Parser.succeed Tuple.pair
        |= Parser.float
        |= unit


timingWithDefault : Parser.Parser Timing
timingWithDefault =
    Parser.oneOf
        [ Parser.map (\_ -> ( 0, Microsecond )) (Parser.symbol "--")
        , timing
        ]


fieldValue : Parser.Parser ( Timing, Timing )
fieldValue =
    Parser.succeed Tuple.pair
        |= timingWithDefault
        |. Parser.symbol "/"
        |= timing


fieldValues : Parser.Parser (List ( Timing, Timing ))
fieldValues =
    Parser.sequence
        { start = "("
        , separator = ","
        , end = ")"
        , spaces = noLineSpaces
        , item = fieldValue
        , trailing = Parser.Forbidden
        }


nodeName : Parser.Parser (List String)
nodeName =
    Parser.sequence
        { start = ""
        , separator = "/"
        , end = ""
        , spaces = noLineSpaces
        , item = identifier
        , trailing = Parser.Forbidden
        }


indentStep : Int
indentStep =
    2


treeList : Parser.Parser (List Tree)
treeList =
    Parser.loop [] treeListHelp


treeListHelp : List Tree -> Parser.Parser (Parser.Step (List Tree) (List Tree))
treeListHelp trees =
    Parser.oneOf
        [ Parser.succeed (\t -> Loop (t :: trees))
            |= tree
        , Parser.succeed ()
            |> Parser.map (\_ -> Done (List.reverse trees))
        ]


indent : Parser.Parser ()
indent =
    Parser.getIndent
        |> Parser.andThen (\n -> Parser.symbol (String.repeat n " "))


addIndent : Int -> Parser.Parser a -> Parser.Parser a
addIndent n parser =
    Parser.getIndent
        |> Parser.andThen (\i -> Parser.withIndent (i + n) parser)


tree : Parser.Parser Tree
tree =
    Parser.succeed Tree
        |. indent
        |= nodeName
        |. noLineSpaces
        |= fieldValues
        |= Parser.oneOf
            [ Parser.succeed identity
                |. Parser.chompIf (\c -> c == '\n')
                |= addIndent indentStep treeList
            , Parser.succeed []
                |. Parser.end
            ]


profile : Parser.Parser Profile
profile =
    Parser.succeed Profile
        |. header
        |. Parser.spaces
        |= fieldNames
        |. Parser.spaces
        |= Parser.withIndent 0 tree
        |. Parser.spaces
        |. Parser.end
