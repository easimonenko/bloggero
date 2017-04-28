module Blog.PostInfo exposing (PostInfo, postInfoDecoder)

import Json.Decode exposing (..)


type alias PostInfo =
    { author : Maybe String
    , abstract : Maybe String
    , date : Maybe String
    }


postInfoDecoder : Decoder (Maybe PostInfo)
postInfoDecoder =
    maybe
        (field "post"
            (map3 PostInfo
                (maybe (field "author" string))
                (maybe (field "abstract" string))
                (maybe (field "date" string))
            )
        )
