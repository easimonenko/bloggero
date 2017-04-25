module Alert.InPlaceAlertClosable exposing (Model, Msg, init, update, view)

import Html
import Html.Attributes exposing (class)


-- Material Desing Lite modules

import Material
import Material.Button as Button
import Material.Icon as Icon
import Material.Options as MdlOptions


-- Bloggero modules

import Alert.AlertLevel as AlertLevel


type alias Model =
    { mdl : Material.Model
    , level : AlertLevel.Level
    , message : String
    , closed : Bool
    }


type Msg
    = Mdl (Material.Msg Msg)
    | AlertClose


init : AlertLevel.Level -> String -> Model
init level message =
    { mdl = Material.model
    , level = level
    , message = message
    , closed = False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl mdlMsg ->
            Material.update Mdl mdlMsg model

        AlertClose ->
            ( { model | closed = True }
            , Cmd.none
            )


view : Model -> Html.Html Msg
view model =
    if not model.closed then
        Html.div [ class "alert" ]
            [ Html.p [ class <| AlertLevel.toCSSClassName model.level ]
                [ Html.text model.message
                , Button.render Mdl
                    [ 0 ]
                    model.mdl
                    [ Button.icon, MdlOptions.onClick AlertClose ]
                    [ Icon.i "close" ]
                ]
            ]
    else
        Html.text ""
