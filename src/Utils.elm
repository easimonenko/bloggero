module Utils exposing (pagePath, toHumanReadable, tuple2triple, stringToMaybe)

import Http
import Navigation
import String
import Tuple


pagePath : Navigation.Location -> String
pagePath location =
    String.dropLeft 2 location.hash


toHumanReadable : Http.Error -> String
toHumanReadable error =
    "HTTP request error "
        ++ case error of
            Http.BadUrl info ->
                "<BadUrl> " ++ info

            Http.Timeout ->
                "<Timeout>"

            Http.NetworkError ->
                "<NetworkError>"

            Http.BadStatus response ->
                "<BadStatus> "
                    ++ "["
                    ++ (response.status.code |> toString)
                    ++ "] "
                    ++ response.status.message

            Http.BadPayload info _ ->
                "<BadPayload> " ++ info


tuple2triple : ( a, b ) -> c -> ( a, b, c )
tuple2triple t v =
    ( Tuple.first t, Tuple.second t, v )


stringToMaybe : String -> Maybe String
stringToMaybe str =
    if String.isEmpty str then
        Nothing
    else
        Just str
