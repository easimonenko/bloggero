module News.NewsInfo exposing (NewsInfo, newsInfoDecoder)

import Json.Decode exposing (..)


type alias NewsInfo =
    { author : Maybe String
    , date : Maybe String
    }


newsInfoDecoder : Decoder (Maybe NewsInfo)
newsInfoDecoder =
    maybe
        (field "news"
            (map2 NewsInfo
                (maybe (field "author" string))
                (maybe (field "date" string))
            )
        )
