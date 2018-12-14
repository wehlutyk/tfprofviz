module Types exposing (Profile, Timing, Tree(..), Triple, Unit(..), first, second, third, toList, toTriple)


type Unit
    = Millisecond
    | Microsecond


type alias Timing =
    ( Float, Unit )


type Triple a
    = Triple a a a


toTriple : List a -> Maybe (Triple a)
toTriple list =
    case list of
        [ field1, field2, field3 ] ->
            Triple field1 field2 field3
                |> Just

        _ ->
            Nothing


toList : Triple a -> List a
toList (Triple field1 field2 field3) =
    [ field1, field2, field3 ]


first : Triple a -> a
first (Triple field1 _ _) =
    field1


second : Triple a -> a
second (Triple _ field2 _) =
    field2


third : Triple a -> a
third (Triple _ _ field3) =
    field3


type alias Profile =
    { fieldNames : Triple String
    , tree : Tree
    }


type Tree
    = Tree (List String) (Triple ( Timing, Timing )) (List Tree)
