module Blog.PostInfo exposing (PostInfo, postInfoDecoder)

import Json.Decode exposing (..)


type alias PostInfo =
    { author : Maybe String
    , abstract : Maybe String
    , date : Maybe String
    , updatingDate : Maybe String
    }


postInfoDecoder : Decoder (Maybe PostInfo)
postInfoDecoder =
    maybe
        (field "post"
            (map4 PostInfo
                (maybe (field "author" string))
                (maybe (field "abstract" string))
                (maybe (field "date" string))
                (maybe (field "updating_date" string))
            )
        )
