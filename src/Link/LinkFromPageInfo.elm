module Link.LinkFromPageInfo exposing (linkFromPageInfo)

import Html
import Html.Attributes exposing (..)


-- Bloggero modules

import Page.PageInfo as PageInfo


linkFromPageInfo : String -> PageInfo.PageInfo -> Html.Html msg
linkFromPageInfo path pageInfo =
    Html.a [ href <| "/#!" ++ path ] [ Html.text pageInfo.title ]
