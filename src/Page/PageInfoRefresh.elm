module Page.PageInfoRefresh exposing (Model, Msg, OutMsg(..), init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)


-- Material Design Lite modules

import Material
import Material.Button as MdlButton
import Material.Color as MdlColor
import Material.Options as MdlOptions


-- Bloggero modules

import Utils


type alias Model =
    { mdl : Material.Model
    , title : String
    , info : String
    }


type Msg
    = Mdl (Material.Msg Msg)
    | ButtonRefresh


type OutMsg
    = PageInfoRefresh
    | NoneOutMsg


init : String -> String -> ( Model, Cmd Msg, OutMsg )
init title info =
    ( { mdl = Material.model
      , title = title
      , info = info
      }
    , Cmd.none
    , NoneOutMsg
    )


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        Mdl mdlMsg ->
            Utils.tuple2triple (Material.update mdlMsg model) NoneOutMsg

        ButtonRefresh ->
            ( model, Cmd.none, PageInfoRefresh )


view : Model -> Html Msg
view model =
    MdlOptions.div [ MdlOptions.cs "mdl-card mdl-shadow--2dp" ]
        [ MdlOptions.div [ MdlOptions.cs "mdl-card__title" ]
            [ h1 [ class "mdl-card__title-text" ] [ text model.title ]
            ]
        , MdlOptions.div
            [ MdlOptions.cs "mdl-card__supporting-text"
            , MdlColor.background (MdlColor.color MdlColor.Yellow MdlColor.S50)
            ]
            [ text model.info
            ]
        , MdlOptions.div [ MdlOptions.cs "mdl-card__actions mdl-card--border" ]
            [ MdlButton.render Mdl
                [ 0 ]
                model.mdl
                [ MdlButton.raised
                , MdlButton.colored
                , MdlButton.ripple
                , MdlButton.onClick ButtonRefresh
                ]
                [ text "Refresh"
                ]
            ]
        ]
