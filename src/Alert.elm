module Alert exposing (Model, Msg(..), Level(..), init, update, view)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Material


type alias Model =
    { mdl : Material.Model
    , level : Level
    , message : String
    }


type Level
    = SuccessLevel
    | InfoLevel
    | WarningLevel
    | DangerLevel


type Msg
    = Mdl (Material.Msg Msg)


init : Level -> String -> ( Model, Cmd Msg )
init level message =
    ( { mdl = Material.model
      , level = level
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
        alertLevel level =
            case level of
                SuccessLevel ->
                    "success"

                InfoLevel ->
                    "info"

                WarningLevel ->
                    "warning"

                DangerLevel ->
                    "danger"
    in
        div
            [ class "alert"
            ]
            [ p
                [ class <| "alert-" ++ (alertLevel model.level)
                ]
                [ text model.message
                ]
            ]
