module Page.Breadcrumbs exposing (Model, Msg, init, update, view)

import Html
import Html.Attributes


-- Bloggero modules

import Page.PageInfo as PageInfo


type alias Model =
    { path : String
    , pageInfo : Maybe PageInfo.PageInfo
    }


type Msg
    = PageInfoMsg PageInfo.Msg


init path =
    ( { path = path, pageInfo = Nothing }
    , Cmd.map PageInfoMsg <| PageInfo.init path
    )


update msg model =
    case msg of
        PageInfoMsg msg ->
            case PageInfo.update msg of
                PageInfo.Success pageInfoJson pageInfo ->
                    ( { model | pageInfo = Just pageInfo }
                    , Cmd.none
                    )

                PageInfo.FetchFail httpError ->
                    ( model, Cmd.none )

                PageInfo.BadJson pageInfoJson errorInfo ->
                    ( model, Cmd.none )


view model =
    Maybe.withDefault (Html.text "") <|
        Maybe.map
            (\pageInfo ->
                Html.div [ Html.Attributes.class "breadcrumbs" ] [ Html.text pageInfo.title ]
            )
            model.pageInfo
