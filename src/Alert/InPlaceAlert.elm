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
    div [ class "alert" ]
        [ p [ class <| AlertLevel.toCSSClassName model.level ]
            [ text model.message
            ]
        ]
