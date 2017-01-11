module Page.MarkdownPage exposing (Model, Msg, OutMsg(..), init, update, view)

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
    , content : Maybe String
    , inPlaceAlert : Maybe InPlaceAlert.Model
    }


type alias Options =
    { highlight : Bool }


type Msg
    = PageContentFetchSucceed String
    | PageContentFetchFail Http.Error
    | InPlaceAlertMsg InPlaceAlert.Msg


type OutMsg
    = AlertOutMsg AlertLevel.Level String
    | NoneOutMsg


defaultOptions : Options
defaultOptions =
    { highlight = False }


init : Navigation.Location -> String -> ( Model, Cmd Msg )
init location json =
    let
        ( inPlaceAlert, inPlaceAlertCmds ) =
            InPlaceAlert.init AlertLevel.InfoLevel "Page content loading ..."

        model =
            { location = location
            , options = defaultOptions
            , content = Nothing
            , inPlaceAlert = Just inPlaceAlert
            }

        optionsDecoder =
            decode identity
                |> optional "markdown"
                    (decode Options |> optional "highlight" bool False)
                    defaultOptions
    in
        case decodeString optionsDecoder json of
            Ok options ->
                ( { model | options = options }
                , Cmd.batch
                    [ Task.attempt
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
                    , Cmd.map InPlaceAlertMsg <| inPlaceAlertCmds
                    ]
                )

            Err error ->
                let
                    ( inPlaceAlert, inPlaceAlertCmds ) =
                        InPlaceAlert.init AlertLevel.DangerLevel error
                in
                    ( { model | inPlaceAlert = Just inPlaceAlert }
                    , Cmd.map InPlaceAlertMsg inPlaceAlertCmds
                    )


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        PageContentFetchSucceed pageContent ->
            if String.isEmpty pageContent then
                let
                    ( inPlaceAlert, inPlaceAlertCmds ) =
                        InPlaceAlert.init AlertLevel.WarningLevel "Page content is empty."
                in
                    ( { model | content = Just pageContent, inPlaceAlert = Just inPlaceAlert }
                    , Cmd.map InPlaceAlertMsg inPlaceAlertCmds
                    , NoneOutMsg
                    )
            else
                ( { model | content = Just pageContent, inPlaceAlert = Nothing }
                , Cmd.none
                , NoneOutMsg
                )

        PageContentFetchFail httpError ->
            let
                ( inPlaceAlert, inPlaceAlertCmds ) =
                    InPlaceAlert.init AlertLevel.DangerLevel "Page content not found."
            in
                ( { model | inPlaceAlert = Just inPlaceAlert }
                , Cmd.map InPlaceAlertMsg inPlaceAlertCmds
                , AlertOutMsg AlertLevel.DangerLevel <| Utils.toHumanReadable httpError
                )

        InPlaceAlertMsg inPlaceAlertMsg ->
            case model.inPlaceAlert of
                Just inPlaceAlert ->
                    let
                        ( inPlaceAlertUpdated, inPlaceAlertCmds ) =
                            InPlaceAlert.update inPlaceAlertMsg inPlaceAlert
                    in
                        ( { model | inPlaceAlert = Just inPlaceAlertUpdated }
                        , Cmd.map InPlaceAlertMsg inPlaceAlertCmds
                        , NoneOutMsg
                        )

                Nothing ->
                    ( model, Cmd.none, NoneOutMsg )


view : Model -> Html.Html Msg
view model =
    Html.article []
        [ Maybe.withDefault (Html.text "") <|
            flip Maybe.map
                model.inPlaceAlert
                (Html.map InPlaceAlertMsg << InPlaceAlert.view)
        , Maybe.withDefault (Html.text "") <|
            flip Maybe.map
                model.content
                (Markdown.toHtml [])
        ]
