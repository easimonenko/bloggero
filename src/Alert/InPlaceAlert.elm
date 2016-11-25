module Alert.InPlaceAlert exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (class)
import Alert.AlertLevel as AlertLevel


type alias Model =
    { level : AlertLevel.Level
    , message : String
    }


type Msg
    = NoneMsg


init : AlertLevel.Level -> String -> ( Model, Cmd Msg )
init level message =
    ( { level = level
      , message = message
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    let
        level =
            case model.level of
                AlertLevel.SuccessLevel ->
                    "success"

                AlertLevel.InfoLevel ->
                    "info"

                AlertLevel.WarningLevel ->
                    "warning"

                AlertLevel.DangerLevel ->
                    "danger"

                AlertLevel.NoneLevel ->
                    "none"
    in
        div [ class "alert" ]
            [ p [ class ("alert-" ++ level) ]
                [ text model.message
                ]
            ]
