module Link exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode exposing (..)
import Task


-- Bloggero modules

import Alert.InPlaceAlert as InPlaceAlert
import Alert.AlertLevel as AlertLevel
import Utils


type alias Model =
    { pagePath : String
    , pageTitle : String
    , inPlaceAlert : Maybe InPlaceAlert.Model
    }


type Msg
    = PageInfoFetchSucceed String
    | PageInfoFetchFail Http.Error
    | InPlaceAlertMsg InPlaceAlert.Msg


init : String -> ( Model, Cmd Msg )
init pagePath =
    ( { pagePath = pagePath, pageTitle = "", inPlaceAlert = Nothing }
    , Task.attempt
        (\result ->
            case result of
                Ok pageInfo ->
                    PageInfoFetchSucceed pageInfo

                Err error ->
                    PageInfoFetchFail error
        )
        (Http.toTask <| Http.getString <| pagePath ++ "/index.json")
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageInfoFetchSucceed pageInfo ->
            let
                decoder =
                    field "title" string
            in
                case decodeString decoder pageInfo of
                    Ok pageTitle ->
                        ( { model | pageTitle = pageTitle }, Cmd.none )

                    Err info ->
                        let
                            ( inPlaceAlert, inPlaceAlertCmds ) =
                                InPlaceAlert.init AlertLevel.DangerLevel info
                        in
                            ( { model | inPlaceAlert = Just inPlaceAlert }
                            , Cmd.map InPlaceAlertMsg inPlaceAlertCmds
                            )

        PageInfoFetchFail error ->
            let
                ( inPlaceAlert, inPlaceAlertCmds ) =
                    InPlaceAlert.init AlertLevel.DangerLevel (Utils.toHumanReadable error)
            in
                ( { model | inPlaceAlert = Just inPlaceAlert }
                , Cmd.map InPlaceAlertMsg inPlaceAlertCmds
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
                        )

                Nothing ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model.inPlaceAlert of
        Nothing ->
            a [ href <| "/#!" ++ model.pagePath ] [ text model.pageTitle ]

        Just inPlaceAlert ->
            Html.map InPlaceAlertMsg (InPlaceAlert.view inPlaceAlert)
