module Page.HomePage exposing (Model, Msg, Config, init, update, view, defaultConfig)

import Html exposing (..)
import Tuple exposing (..)


-- Material Design Lite modules

import Material


-- Bloggero modules

import Alert.AlertLevel as AlertLevel
import Blog.PostList as PostList


type alias Model =
    { postList : PostList.Model
    , title : String
    , mdl : Material.Model
    }


type Msg
    = PostListMsg PostList.Msg
    | Mdl (Material.Msg Msg)


type OutMsg
    = AlertOutMsg AlertLevel.Level String
    | NoneOutMsg


type alias Config =
    { root : String
    , title : String
    , blogRoot : String
    }


init : Config -> ( Model, Cmd Msg, OutMsg )
init config =
    let
        ( postList, postListCmds, postListOutMsg ) =
            let
                defaultConfig =
                    PostList.defaultConfig
            in
                PostList.init { defaultConfig | root = config.blogRoot }

        outMsg =
            case postListOutMsg of
                PostList.NoneOutMsg ->
                    NoneOutMsg

                PostList.AlertOutMsg level message ->
                    AlertOutMsg level message
    in
        ( { postList = postList, title = config.title, mdl = Material.model }
        , Cmd.map PostListMsg postListCmds
        , outMsg
        )


defaultConfig : Config
defaultConfig =
    { title = "Home"
    , root = "/home"
    , blogRoot = "/blog"
    }


tuple2triple : ( a, b ) -> c -> ( a, b, c )
tuple2triple t v =
    ( first t, second t, v )


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        PostListMsg postListMsg ->
            let
                ( postList, postListCmds, postListOutMsg ) =
                    PostList.update postListMsg model.postList

                outMsg =
                    case postListOutMsg of
                        PostList.NoneOutMsg ->
                            NoneOutMsg

                        PostList.AlertOutMsg level message ->
                            AlertOutMsg level message
            in
                ( { model | postList = postList }, Cmd.map PostListMsg postListCmds, outMsg )

        Mdl mdlMsg ->
            tuple2triple (Material.update Mdl mdlMsg model) NoneOutMsg


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text model.title ]
        , Html.map PostListMsg (PostList.view model.postList)
        ]
