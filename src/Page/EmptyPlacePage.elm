module Page.EmptyPlacePage exposing (Model, Msg, init, update, view)

import Html exposing (..)


type alias Model =
    ()


type Msg
    = NoneMsg


init : ( Model, Cmd Msg )
init =
    ( (), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoneMsg ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    text ""
