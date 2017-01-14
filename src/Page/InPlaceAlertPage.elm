module Page.InPlaceAlertPage exposing (Model, Msg, init, update, view)

import Html exposing (..)


-- Bloggero modules

import Alert.AlertLevel as AlertLevel
import Alert.InPlaceAlert as InPlaceAlert


type alias Model =
    { inPlaceAlert : InPlaceAlert.Model }


type alias Msg =
    ()


init : AlertLevel.Level -> String -> ( Model, Cmd Msg )
init level message =
    let
        inPlaceAlert =
            InPlaceAlert.init level message
    in
        ( { inPlaceAlert = inPlaceAlert }
        , Cmd.none
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    InPlaceAlert.view model.inPlaceAlert
