module Page.HtmlPage exposing (Model, Msg, init, update, view)

import Html
import Http
import Json.Decode exposing (..)
import Markdown
import Navigation
import Task


-- Bloggero modules

import Utils


type alias Model =
    { location : Navigation.Location
    , content : String
    }


type Msg
    = PageInfoFetchSucceed String
    | PageInfoFetchFail Http.Error
    | PageContentFetchSucceed String
    | PageContentFetchFail Http.Error


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( { location = location
      , content = ""
      }
    , Task.attempt
        (\result ->
            case result of
                Ok pageInfo ->
                    PageInfoFetchSucceed pageInfo

                Err error ->
                    PageInfoFetchFail error
        )
        (Http.toTask <| Http.getString <| (Utils.pagePath location) ++ "/index.json")
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageInfoFetchSucceed pageInfo ->
            case decodeString (maybe (field "type" string)) pageInfo of
                Ok contentType ->
                    case Maybe.withDefault "markdown" contentType of
                        "html" ->
                            ( model
                            , Task.attempt
                                (\result ->
                                    case result of
                                        Ok content ->
                                            PageContentFetchSucceed content

                                        Err error ->
                                            PageContentFetchFail error
                                )
                                (Http.toTask <|
                                    Http.getString <|
                                        (Utils.pagePath model.location)
                                            ++ "/index.html"
                                )
                            )

                        _ ->
                            ( model, Cmd.none )

                Err error ->
                    ( model, Cmd.none )

        PageContentFetchSucceed pageContent ->
            ( { model | content = pageContent }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.article [] [ Markdown.toHtml [] model.content ]
