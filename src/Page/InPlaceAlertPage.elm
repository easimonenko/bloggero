module Page.InPlaceAlertPage exposing (Model, Msg, init, update, view)

import Html exposing (..)


-- Bloggero modules

import Alert.AlertLevel as AlertLevel
import Alert.InPlaceAlert as InPlaceAlert


type alias Model =
    { inPlaceAlert : InPlaceAlert.Model }


type Msg
    = InPlaceAlertMsg InPlaceAlert.Msg


init : AlertLevel.Level -> String -> ( Model, Cmd Msg )
init level message =
    let
        ( inPlaceAlert, inPlaceAlertCmds ) =
            InPlaceAlert.init level message
    in
        ( { inPlaceAlert = inPlaceAlert }
        , Cmd.map InPlaceAlertMsg inPlaceAlertCmds
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InPlaceAlertMsg inPlaceAlertMsg ->
            let
                ( inPlaceAlert, inPlaceAlertCmds ) =
                    InPlaceAlert.update inPlaceAlertMsg model.inPlaceAlert
            in
                ( { model | inPlaceAlert = inPlaceAlert }
                , Cmd.map InPlaceAlertMsg inPlaceAlertCmds
                )


view : Model -> Html Msg
view model =
    Html.map InPlaceAlertMsg (InPlaceAlert.view model.inPlaceAlert)
