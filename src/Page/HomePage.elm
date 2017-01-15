module Page.HomePage exposing (Model, Msg, Config, init, update, view, defaultConfig)

import Html


-- Material Design Lite modules

import Material


-- Bloggero modules

import Alert.AlertLevel as AlertLevel
import Blog.PostList as PostList
import News.NewsList as NewsList
import Utils


type alias Model =
    { postList : PostList.Model
    , newsList : NewsList.Model
    , config : Config
    , mdl : Material.Model
    }


type Msg
    = PostListMsg PostList.Msg
    | NewsListMsg NewsList.Msg
    | Mdl (Material.Msg Msg)


type OutMsg
    = AlertOutMsg AlertLevel.Level String
    | NoneOutMsg


type alias Config =
    { root : Path
    , title : String
    , blogRoot : Path
    , newsRoot : Path
    }


type alias Path =
    String


defaultConfig : Config
defaultConfig =
    { title = "Home"
    , root = "/home"
    , blogRoot = "/blog"
    , newsRoot = "/news"
    }


init : Config -> ( Model, Cmd Msg, OutMsg )
init config =
    let
        ( postList, postListCmds, postListOutMsg ) =
            let
                postListDefaultConfig =
                    PostList.defaultConfig
            in
                PostList.init { postListDefaultConfig | root = config.blogRoot }

        ( newsList, newsListCmds ) =
            let
                newsListDefaultConfig =
                    NewsList.defaultConfig
            in
                NewsList.init { newsListDefaultConfig | root = config.newsRoot }

        outMsg =
            case postListOutMsg of
                PostList.NoneOutMsg ->
                    NoneOutMsg

                PostList.AlertOutMsg level message ->
                    AlertOutMsg level message
    in
        ( { postList = postList, newsList = newsList, config = config, mdl = Material.model }
        , Cmd.batch
            [ Cmd.map PostListMsg postListCmds
            , Cmd.map NewsListMsg newsListCmds
            ]
        , outMsg
        )


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
                ( { model | postList = postList }
                , Cmd.map PostListMsg postListCmds
                , outMsg
                )

        NewsListMsg newsListMsg ->
            let
                ( newsList, newsListCmds ) =
                    NewsList.update newsListMsg model.newsList
            in
                ( { model | newsList = newsList }
                , Cmd.map NewsListMsg newsListCmds
                , NoneOutMsg
                )

        Mdl mdlMsg ->
            Utils.tuple2triple (Material.update Mdl mdlMsg model) NoneOutMsg


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.h1 [] [ Html.text model.config.title ]
        , Html.map NewsListMsg <| NewsList.view model.newsList
        , Html.map PostListMsg <| PostList.view model.postList
        ]
