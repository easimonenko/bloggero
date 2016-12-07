module Blog.BlogPage exposing (Model, Msg, init, update, view, defaultConfig)

import Html exposing (..)


-- Bloggero modules

import Alert.AlertLevel as AlertLevel
import Blog.PostList as PostList


type alias Model =
    { title : String
    , root : String
    , postList : PostList.Model
    }


type Msg
    = PostListMsg PostList.Msg


type OutMsg
    = AlertOutMsg AlertLevel.Level String
    | NoneOutMsg


type alias Config =
    { title : String
    , root : String
    }


init : Config -> ( Model, Cmd Msg, OutMsg )
init config =
    let
        ( postList, postListCmds, postListOutMsg ) =
            let
                defaultConfig =
                    PostList.defaultConfig
            in
                PostList.init defaultConfig
    in
        ( { title = config.title
          , root = config.root
          , postList = postList
          }
        , Cmd.map PostListMsg postListCmds
        , NoneOutMsg
        )


defaultConfig : Config
defaultConfig =
    { title = "Blog"
    , root = "/blog"
    }


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


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text model.title ]
        , Html.map PostListMsg (PostList.view model.postList)
        ]
