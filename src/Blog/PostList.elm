module Blog.PostList exposing (Model, Msg, OutMsg(..), init, update, view)

import Html exposing (..)
import Html.App
import Http
import Json.Decode exposing (..)
import Task


-- Material Design Lite modules

import Material
import Material.List as MdlList


-- Bloggero modules

import Alert.Alert as Alert
import Alert.AlertLevel as AlertLevel
import Alert.InPlaceAlert as InPlaceAlert


type alias Model =
    { mdl : Material.Model
    , root : String
    , postList : List String
    , inPlaceAlert : Maybe InPlaceAlert.Model
    , alertInfo : String
    , alertLevel : AlertLevel.Level
    }


type Msg
    = Mdl (Material.Msg Msg)
    | BlogConfigFetchFail Http.Error
    | BlogConfigFetchSucceed String
    | InPlaceAlertMsg InPlaceAlert.Msg


type OutMsg
    = NoneOutMsg
    | AlertOutMsg AlertLevel.Level String


init : String -> ( Model, Cmd Msg, OutMsg )
init root =
    ( { mdl = Material.model
      , root = root
      , postList = []
      , inPlaceAlert = Nothing
      , alertInfo = ""
      , alertLevel = AlertLevel.NoneLevel
      }
    , Task.perform BlogConfigFetchFail BlogConfigFetchSucceed (Http.getString <| root ++ "/index.json")
    , NoneOutMsg
    )


tuple2triple : ( a, b ) -> c -> ( a, b, c )
tuple2triple t v =
    ( fst t, snd t, v )


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        Mdl mdlMsg ->
            tuple2triple (Material.update mdlMsg model) NoneOutMsg

        BlogConfigFetchSucceed config ->
            let
                postListResult =
                    decodeString ("blog" := (object1 identity ("posts" := list string))) config
            in
                case postListResult of
                    Ok postList ->
                        ( { model | postList = postList }, Cmd.none, NoneOutMsg )

                    Err info ->
                        ( { model | alertInfo = info, alertLevel = AlertLevel.DangerLevel }, Cmd.none, AlertOutMsg AlertLevel.DangerLevel info )

        _ ->
            ( model, Cmd.none, NoneOutMsg )


view : Model -> Html Msg
view model =
    case model.inPlaceAlert of
        Nothing ->
            MdlList.ul [] <|
                List.map (\postId -> MdlList.li [] [ text postId ]) model.postList

        Just inPlaceAlert ->
            Html.App.map InPlaceAlertMsg (InPlaceAlert.view inPlaceAlert)
