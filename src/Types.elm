module Types exposing (Profile, Timing, Tree(..), Unit(..))


type Unit
    = Millisecond
    | Microsecond


type alias Timing =
    ( Float, Unit )


type alias Profile =
    { fieldNames : List String
    , tree : Tree
    }


type Tree
    = Tree (List String) (List ( Timing, Timing )) (List Tree)
