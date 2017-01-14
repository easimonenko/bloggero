module Alert.InPlaceAlert exposing (Model, init, view)

import Html exposing (..)
import Html.Attributes exposing (class)
import Alert.AlertLevel as AlertLevel


type alias Model =
    { level : AlertLevel.Level
    , message : String
    }


init : AlertLevel.Level -> String -> Model
init level message =
    { level = level
    , message = message
    }


view : Model -> Html msg
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
