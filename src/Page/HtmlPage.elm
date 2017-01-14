module Page.HtmlPage exposing (Model, Msg, init, update, view)

import Html
import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Markdown
import Navigation
import Task


-- Bloggero modules

import Alert.AlertLevel as AlertLevel
import Alert.InPlaceAlert as InPlaceAlert
import Utils


type alias Model =
    { location : Navigation.Location
    , options : Options
    , content : String
    , inPlaceAlert : Maybe InPlaceAlert.Model
    }


type alias Options =
    { highlight : Bool }


type Msg
    = PageInfoFetchSucceed String
    | PageInfoFetchFail Http.Error
    | PageContentFetchSucceed String
    | PageContentFetchFail Http.Error


defaultOptions : Options
defaultOptions =
    { highlight = False }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( { location = location
      , options = defaultOptions
      , content = ""
      , inPlaceAlert = Nothing
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
            let
                optionsDecoder =
                    decode identity
                        |> optional "html"
                            (decode Options |> optional "highlight" bool False)
                            defaultOptions
            in
                case decodeString optionsDecoder pageInfo of
                    Ok options ->
                        ( { model | options = options }
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

                    Err error ->
                        let
                            inPlaceAlert =
                                InPlaceAlert.init AlertLevel.DangerLevel error
                        in
                            ( { model | inPlaceAlert = Just inPlaceAlert }
                            , Cmd.none
                            )

        PageContentFetchSucceed pageContent ->
            ( { model | content = pageContent }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.article []
        [ model.inPlaceAlert
            |> Maybe.map InPlaceAlert.view
            |> Maybe.withDefault (Html.text "")
        , Markdown.toHtml [] model.content
        ]
