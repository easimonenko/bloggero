module Utils exposing (pagePath, tuple2triple)

import Navigation
import Tuple


pagePath : Navigation.Location -> String
pagePath location =
    String.dropLeft 2 location.hash


tuple2triple : ( a, b ) -> c -> ( a, b, c )
tuple2triple t v =
    ( Tuple.first t, Tuple.second t, v )
