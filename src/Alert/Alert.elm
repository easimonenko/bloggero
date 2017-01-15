module Alert.Alert exposing (Model, Msg(..), init, update, view)

import Html exposing (..)


-- Material Design Lite modules

import Material
import Material.Button as Button
import Material.Icon as Icon
import Material.List as MdlList
import Material.Options as MdlOptions


-- Bloggero modules

import Alert.AlertLevel as AlertLevel


type alias Model =
    { mdl : Material.Model
    , id : Int
    , level : AlertLevel.Level
    , message : String
    }


type Msg
    = Mdl (Material.Msg Msg)
    | AlertClose Int


init : AlertLevel.Level -> Int -> String -> ( Model, Cmd Msg )
init level id message =
    ( { mdl = Material.model
      , id = id
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
    MdlList.li [ MdlOptions.cs <| AlertLevel.toCSSClassName model.level ]
        [ MdlList.content [] [ text model.message ]
        , MdlList.content2 []
            [ Button.render Mdl
                [ 0 ]
                model.mdl
                [ Button.icon, MdlOptions.onClick (AlertClose model.id) ]
                [ Icon.i "close" ]
            ]
        ]
