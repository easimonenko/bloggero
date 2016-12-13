module Blog.PostList exposing (Model, Msg, OutMsg(..), init, update, view, defaultConfig)

import Dict exposing (..)
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
import Link


type alias Model =
    { --mdl : Material.Model
      --, root : String
      root : String
    , title : String
    , postIds : List String
    , postListLinks : Dict String Link.Model
    , inPlaceAlert : Maybe InPlaceAlert.Model
    }


type Msg
    = --Mdl (Material.Msg Msg)
      -- | BlogConfigFetchFail Http.Error
      BlogConfigFetchFail Http.Error
    | BlogConfigFetchSucceed String
    | LinkMsg String Link.Msg
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
    ( { --mdl = Material.model
        --, root = config.root
        root = config.root
      , title = config.title
      , postIds = []
      , postListLinks = Dict.empty
      , inPlaceAlert = Nothing
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
        --Mdl mdlMsg ->
        --    tuple2triple (Material.update mdlMsg model) NoneOutMsg
        BlogConfigFetchSucceed config ->
            let
                configDecoder =
                    field "blog" <|
                        Json.Decode.map identity <|
                            field "posts" <|
                                Json.Decode.list string

                configDecodeResult =
                    decodeString configDecoder config
            in
                case configDecodeResult of
                    Ok postIds ->
                        let
                            ( links, linkCmds ) =
                                List.unzip <|
                                    List.map
                                        (\postId ->
                                            let
                                                ( link, linkCmds ) =
                                                    Link.init <| model.root ++ "/" ++ postId
                                            in
                                                ( ( postId, link ), ( postId, linkCmds ) )
                                        )
                                        postIds
                        in
                            ( { model | postIds = postIds, postListLinks = Dict.fromList links }
                            , Cmd.batch <|
                                List.map
                                    (\( postId, linkCmds ) -> Cmd.map (LinkMsg postId) linkCmds)
                                    linkCmds
                            , NoneOutMsg
                            )

                    Err info ->
                        let
                            ( inPlaceAlert, inPlaceAlertCmds ) =
                                InPlaceAlert.init AlertLevel.DangerLevel info
                        in
                            ( { model | inPlaceAlert = Just inPlaceAlert }
                            , Cmd.map InPlaceAlertMsg inPlaceAlertCmds
                            , AlertOutMsg AlertLevel.DangerLevel info
                            )

        BlogConfigFetchFail error ->
            let
                ( inPlaceAlert, inPlaceAlertCmds ) =
                    InPlaceAlert.init AlertLevel.DangerLevel "Http Error"
            in
                ( { model | inPlaceAlert = Just inPlaceAlert }
                , Cmd.map InPlaceAlertMsg inPlaceAlertCmds
                , AlertOutMsg AlertLevel.DangerLevel "Http Error"
                )

        LinkMsg postId linkMsg ->
            case Dict.get postId model.postListLinks of
                Just link ->
                    let
                        ( linkUpdated, linkCmds ) =
                            Link.update linkMsg link
                    in
                        ( { model
                            | postListLinks =
                                Dict.update postId
                                    (\item ->
                                        case item of
                                            Just _ ->
                                                Just linkUpdated

                                            Nothing ->
                                                Nothing
                                    )
                                    model.postListLinks
                          }
                        , Cmd.map (LinkMsg postId) linkCmds
                        , NoneOutMsg
                        )

                Nothing ->
                    ( model, Cmd.none, NoneOutMsg )

        _ ->
            ( model, Cmd.none, NoneOutMsg )


view : Model -> Html Msg
view model =
    case model.inPlaceAlert of
        Nothing ->
            div []
                [ h2 []
                    [ text model.title ]
                , MdlList.ul [] <|
                    List.map
                        (\postId ->
                            MdlList.li []
                                [ case Dict.get postId model.postListLinks of
                                    Just link ->
                                        Html.map (LinkMsg postId) (Link.view link)

                                    Nothing ->
                                        text ""
                                ]
                        )
                        model.postIds
                ]

        Just inPlaceAlert ->
            Html.map InPlaceAlertMsg (InPlaceAlert.view inPlaceAlert)
