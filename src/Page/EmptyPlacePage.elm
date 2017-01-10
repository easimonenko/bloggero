module Page.EmptyPlacePage exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Navigation


-- Bloggero modules

import Alert.InPlaceAlert
import Alert.AlertLevel
import Utils


type alias Model =
    { location : Navigation.Location
    , inPlaceAlert : Alert.InPlaceAlert.Model
    }


type Msg
    = InPlaceAlertMsg Alert.InPlaceAlert.Msg


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        ( inPlaceAlert, inPlaceAlertCmds ) =
            Alert.InPlaceAlert.init
                Alert.AlertLevel.InfoLevel
                ("Loading of page " ++ (Utils.pagePath location) ++ " ...")
    in
        ( { location = location, inPlaceAlert = inPlaceAlert }
        , Cmd.map InPlaceAlertMsg inPlaceAlertCmds
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InPlaceAlertMsg inPlaceAlertMsg ->
            let
                ( inPlaceAlert, inPlaceAlertCmds ) =
                    Alert.InPlaceAlert.update inPlaceAlertMsg model.inPlaceAlert
            in
                ( { model | inPlaceAlert = inPlaceAlert }
                , Cmd.map InPlaceAlertMsg inPlaceAlertCmds
                )


view : Model -> Html Msg
view model =
    Html.map InPlaceAlertMsg (Alert.InPlaceAlert.view model.inPlaceAlert)
