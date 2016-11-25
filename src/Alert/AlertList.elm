module Alert.AlertList exposing (Model, Msg(..), init, update, view, add)

import Html exposing (..)
import Material
import Material.List as MdlList
import Alert.Alert as Alert
import Alert.AlertLevel as AlertLevel


type alias Model =
    { mdl : Material.Model
    , alerts : List Alert.Model
    , currentId : Int
    }


type Msg
    = Mdl (Material.Msg Msg)
    | AlertMsg Alert.Msg


init : ( Model, Cmd Msg )
init =
    ( { mdl = Material.model
      , alerts = []
      , currentId = 0
      }
    , Cmd.none
    )


add : Model -> AlertLevel.Level -> String -> ( Model, Cmd Msg )
add model level message =
    let
        ( alert, alertCmds ) =
            Alert.init level model.currentId message

        alerts =
            alert :: model.alerts

        currentId =
            model.currentId + 1
    in
        ( { model | alerts = alerts, currentId = currentId }, Cmd.map AlertMsg alertCmds )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl mdlMsg ->
            Material.update mdlMsg model

        AlertMsg alertMsg ->
            case alertMsg of
                Alert.AlertClose alertId ->
                    ( { model | alerts = List.filter (\alert -> alert.id /= alertId) model.alerts }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    MdlList.ul [] <| List.map (Html.map AlertMsg << Alert.view) model.alerts
