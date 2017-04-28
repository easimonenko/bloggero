module Blog.PostList exposing (Model, Msg, OutMsg(..), init, update, view, defaultConfig)

import Dict exposing (..)
import Html
import Html.Attributes exposing (..)
import Html.Attributes.Extra exposing (innerHtml)
import Json.Decode exposing (..)
import Maybe.Extra
import Result exposing (..)


-- Material Design Lite modules

import Material.List as MdlList


-- Bloggero modules

import Alert.AlertLevel as AlertLevel
import Alert.InPlaceAlert as InPlaceAlert
import Blog.PostInfo exposing (..)
import Link.LinkFromPageInfo exposing (..)
import Page.PageInfo as PageInfo
import Utils


type alias Model =
    { config : PostListConfig
    , postIds : List PostId
    , postPageInfos : Dict.Dict PostId PostPageInfo
    , inPlaceAlert : Maybe InPlaceAlert.Model
    }


type alias PostId =
    String


type alias PostPageInfo =
    { pageInfo : PageInfo.PageInfo
    , postInfo : Maybe PostInfo
    }


type Msg
    = PageInfoMsg PageInfo.Msg
    | PostPageInfoMsg PostId PageInfo.Msg


type OutMsg
    = NoneOutMsg
    | AlertOutMsg AlertLevel.Level String


type alias PostListConfig =
    { root : String
    , title : String
    }


init : PostListConfig -> ( Model, Cmd Msg, OutMsg )
init config =
    ( { config = config
      , postIds = []
      , postPageInfos = Dict.empty
      , inPlaceAlert = Nothing
      }
    , Cmd.map PageInfoMsg <| PageInfo.init config.root
    , NoneOutMsg
    )


defaultConfig : PostListConfig
defaultConfig =
    { root = "/blog"
    , title = "Post List"
    }


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        PageInfoMsg pageInfoMsg ->
            case PageInfo.update pageInfoMsg of
                PageInfo.Success _ json pageInfo ->
                    let
                        configDecoder =
                            field "blog" <|
                                Json.Decode.map identity <|
                                    field "posts" <|
                                        Json.Decode.list string

                        configDecodeResult =
                            decodeString configDecoder json
                    in
                        case configDecodeResult of
                            Ok postIds ->
                                if List.isEmpty postIds then
                                    let
                                        info =
                                            "Post list is empty."

                                        inPlaceAlert =
                                            InPlaceAlert.init
                                                AlertLevel.WarningLevel
                                                info
                                    in
                                        ( { model
                                            | inPlaceAlert = Just inPlaceAlert
                                          }
                                        , Cmd.none
                                        , AlertOutMsg AlertLevel.InfoLevel info
                                        )
                                else
                                    let
                                        info =
                                            "Post list loaded."

                                        inPlaceAlert =
                                            InPlaceAlert.init
                                                AlertLevel.SuccessLevel
                                                info

                                        postPageInfoCmds =
                                            List.map
                                                (\postId ->
                                                    Cmd.map (PostPageInfoMsg postId)
                                                        (PageInfo.init <| model.config.root ++ "/" ++ postId)
                                                )
                                                postIds
                                    in
                                        ( { model
                                            | inPlaceAlert = Just inPlaceAlert
                                            , postIds = postIds
                                          }
                                        , Cmd.batch postPageInfoCmds
                                        , AlertOutMsg AlertLevel.InfoLevel info
                                        )

                            Err info ->
                                let
                                    inPlaceAlert =
                                        InPlaceAlert.init AlertLevel.DangerLevel info
                                in
                                    ( { model | inPlaceAlert = Just inPlaceAlert }
                                    , Cmd.none
                                    , AlertOutMsg AlertLevel.DangerLevel info
                                    )

                PageInfo.FetchFail _ error ->
                    let
                        info =
                            "Http Error: " ++ (Utils.toHumanReadable error)

                        inPlaceAlert =
                            InPlaceAlert.init AlertLevel.DangerLevel info
                    in
                        ( { model | inPlaceAlert = Just inPlaceAlert }
                        , Cmd.none
                        , AlertOutMsg AlertLevel.DangerLevel info
                        )

                PageInfo.BadJson _ _ error ->
                    let
                        info =
                            "Blog PageInfo: " ++ error

                        inPlaceAlert =
                            InPlaceAlert.init AlertLevel.DangerLevel info
                    in
                        ( { model | inPlaceAlert = Just inPlaceAlert }
                        , Cmd.none
                        , AlertOutMsg AlertLevel.DangerLevel info
                        )

        PostPageInfoMsg postId pageInfoMsg ->
            case PageInfo.update pageInfoMsg of
                PageInfo.Success path json pageInfo ->
                    case decodeString postInfoDecoder json of
                        Ok postInfo ->
                            let
                                postPageInfo =
                                    { pageInfo = pageInfo
                                    , postInfo = postInfo
                                    }
                            in
                                ( { model
                                    | postPageInfos = Dict.insert postId postPageInfo model.postPageInfos
                                    , inPlaceAlert = Nothing
                                  }
                                , Cmd.none
                                , NoneOutMsg
                                )

                        Err error ->
                            ( model, Cmd.none, NoneOutMsg )

                _ ->
                    ( model, Cmd.none, NoneOutMsg )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.h2 [] [ Html.text model.config.title ]
        , model.inPlaceAlert
            |> Maybe.map InPlaceAlert.view
            |> Maybe.withDefault (Html.text "")
        , MdlList.ul [] <|
            List.map
                (\postId ->
                    MdlList.li []
                        (case Dict.get postId model.postPageInfos of
                            Just { pageInfo, postInfo } ->
                                let
                                    path =
                                        model.config.root ++ "/" ++ postId
                                in
                                    (linkFromPageInfo path pageInfo)
                                        :: (postInfo
                                                |> Maybe.map
                                                    (\{ author, abstract, date } ->
                                                        [ Maybe.Extra.unwrap
                                                            (Html.text "")
                                                            (\date ->
                                                                Html.span
                                                                    [ class "news-link-date", innerHtml "&ndash;&nbsp;" ]
                                                                    [ Html.text date ]
                                                            )
                                                            date
                                                        , Maybe.Extra.unwrap
                                                            (Html.text "")
                                                            (\author ->
                                                                Html.span
                                                                    [ class "news-link-author", innerHtml "&ndash;&nbsp;" ]
                                                                    [ Html.text author ]
                                                            )
                                                            author
                                                        ]
                                                    )
                                                |> Maybe.withDefault []
                                           )

                            Nothing ->
                                [ Html.text "" ]
                        )
                )
                model.postIds
        ]
