module Page.MarkdownPage exposing (Model, Msg, init, update, view)

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
    | InPlaceAlertMsg InPlaceAlert.Msg


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
                        |> optional "markdown"
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
                                        ++ "/index.markdown"
                            )
                        )

                    Err error ->
                        let
                            ( inPlaceAlert, inPlaceAlertCmds ) =
                                InPlaceAlert.init AlertLevel.DangerLevel error
                        in
                            ( { model | inPlaceAlert = Just inPlaceAlert }
                            , Cmd.map InPlaceAlertMsg inPlaceAlertCmds
                            )

        PageContentFetchSucceed pageContent ->
            ( { model | content = pageContent }, Cmd.none )

        InPlaceAlertMsg inPlaceAlertMsg ->
            case model.inPlaceAlert of
                Just inPlaceAlert ->
                    let
                        ( inPlaceAlertUpdated, inPlaceAlertCmds ) =
                            InPlaceAlert.update inPlaceAlertMsg inPlaceAlert
                    in
                        ( { model | inPlaceAlert = Just inPlaceAlertUpdated }
                        , Cmd.map InPlaceAlertMsg inPlaceAlertCmds
                        )

                Nothing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.article []
        [ case model.inPlaceAlert of
            Just inPlaceAlert ->
                Html.map InPlaceAlertMsg <| InPlaceAlert.view inPlaceAlert

            Nothing ->
                Html.text ""
        , Markdown.toHtml [] model.content
        ]
