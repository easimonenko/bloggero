module Blog.PostList exposing (Model, Msg, OutMsg(..), init, update, view, defaultConfig)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode exposing (..)
import Result exposing (..)
import Task
import Tuple exposing (..)


-- Material Design Lite modules

import Material
import Material.List as MdlList


-- Bloggero modules

import Alert.AlertLevel as AlertLevel
import Alert.InPlaceAlert as InPlaceAlert


type alias Model =
    { mdl : Material.Model
    , root : String
    , title : String
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


type alias PostListConfig =
    { root : String
    , title : String
    }


init : PostListConfig -> ( Model, Cmd Msg, OutMsg )
init config =
    ( { mdl = Material.model
      , root = config.root
      , title = config.title
      , postList = []
      , inPlaceAlert = Nothing
      , alertInfo = ""
      , alertLevel = AlertLevel.NoneLevel
      }
    , Task.attempt
        (\result ->
            case result of
                Err msg ->
                    BlogConfigFetchFail msg

                Ok msg ->
                    BlogConfigFetchSucceed msg
        )
        (Http.toTask <| Http.getString <| config.root ++ "/index.json")
    , NoneOutMsg
    )


defaultConfig : PostListConfig
defaultConfig =
    { root = "/blog"
    , title = "Post List"
    }


tuple2triple : ( a, b ) -> c -> ( a, b, c )
tuple2triple t v =
    ( first t, second t, v )


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        Mdl mdlMsg ->
            tuple2triple (Material.update mdlMsg model) NoneOutMsg

        BlogConfigFetchSucceed config ->
            let
                configDecoder =
                    field "blog" <| Json.Decode.map identity <| field "posts" <| Json.Decode.list string

                configDecodeResult =
                    decodeString configDecoder config
            in
                case configDecodeResult of
                    Ok postList ->
                        ( { model | postList = postList }, Cmd.none, NoneOutMsg )

                    Err info ->
                        let
                            ( inPlaceAlert, inPlaceAlertCmds ) =
                                InPlaceAlert.init AlertLevel.DangerLevel info
                        in
                            ( { model | inPlaceAlert = Just inPlaceAlert }
                            , Cmd.map InPlaceAlertMsg inPlaceAlertCmds
                            , AlertOutMsg AlertLevel.DangerLevel info
                            )

        _ ->
            ( model, Cmd.none, NoneOutMsg )


view : Model -> Html Msg
view model =
    case model.inPlaceAlert of
        Nothing ->
            div []
                [ h2 []
                    [ text model.title ]
                , MdlList.ul
                    []
                  <|
                    List.map
                        (\postId ->
                            MdlList.li []
                                [ a [ href <| "/#!" ++ model.root ++ "/" ++ postId ] [ text postId ] ]
                        )
                        model.postList
                ]

        Just inPlaceAlert ->
            Html.map InPlaceAlertMsg (InPlaceAlert.view inPlaceAlert)
