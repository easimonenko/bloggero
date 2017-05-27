module News.NewsList
    exposing
        ( Model
        , Msg
        , Config
        , OutMsg(..)
        , defaultConfig
        , init
        , update
        , view
        )

import Dict
import Html
import Html.Attributes exposing (..)
import Html.Attributes.Extra exposing (innerHtml)
import Json.Decode exposing (..)
import Maybe.Extra


-- Material Design Lite modules

import Material.List as MdlList


-- Bloggero modules

import Alert.AlertLevel as AlertLevel
import Alert.InPlaceAlert as InPlaceAlert
import Link.LinkFromPageInfo exposing (..)
import News.NewsInfo exposing (..)
import Page.PageInfo as PageInfo
import Utils


type alias Model =
    { config : Config
    , newsIds : List NewsId
    , newsPageInfos : Dict.Dict NewsId (Result String NewsPageInfo)
    , inPlaceAlert : Maybe InPlaceAlert.Model
    }


type alias NewsId =
    String


type alias NewsPageInfo =
    { pageInfo : PageInfo.PageInfo
    , newsInfo : Maybe NewsInfo
    }


type Msg
    = PageInfoMsg PageInfo.Msg
    | NewsPageInfoMsg NewsId PageInfo.Msg


type OutMsg
    = NoneOutMsg
    | AlertOutMsg AlertLevel.Level String


type alias Config =
    { root : String
    , title : String
    }


defaultConfig : Config
defaultConfig =
    { root = "/news"
    , title = "News List"
    }


init : Config -> ( Model, Cmd Msg, OutMsg )
init config =
    let
        inPlaceAlert =
            InPlaceAlert.init AlertLevel.InfoLevel "News list loading..."
    in
        ( { config = config
          , newsIds = []
          , newsPageInfos = Dict.empty
          , inPlaceAlert = Just inPlaceAlert
          }
        , Cmd.map PageInfoMsg (PageInfo.init config.root)
        , NoneOutMsg
        )


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        PageInfoMsg msg ->
            case PageInfo.update msg of
                PageInfo.Success path json pageInfo ->
                    let
                        newsIdsListDecoder =
                            maybe
                                (field "news" <|
                                    Json.Decode.map identity <|
                                        field "news" <|
                                            Json.Decode.list string
                                )

                        newsIdsListDecodeResult =
                            decodeString newsIdsListDecoder json
                    in
                        case newsIdsListDecodeResult of
                            Ok newsIds ->
                                let
                                    newsIdsUnwraped =
                                        newsIds |> Maybe.Extra.unwrap [] identity
                                in
                                    if List.isEmpty newsIdsUnwraped then
                                        let
                                            info =
                                                "News list is empty."

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
                                            inPlaceAlert =
                                                InPlaceAlert.init
                                                    AlertLevel.SuccessLevel
                                                    "News list loaded."

                                            newsPageInfoCmds =
                                                List.map
                                                    (\newsId ->
                                                        Cmd.map (NewsPageInfoMsg newsId)
                                                            (PageInfo.init <| model.config.root ++ "/" ++ newsId)
                                                    )
                                                    newsIdsUnwraped
                                        in
                                            ( { model
                                                | newsIds = newsIdsUnwraped
                                                , inPlaceAlert = Just inPlaceAlert
                                              }
                                            , Cmd.batch newsPageInfoCmds
                                            , NoneOutMsg
                                            )

                            Err info ->
                                let
                                    inPlaceAlert =
                                        InPlaceAlert.init AlertLevel.DangerLevel info
                                in
                                    ( { model
                                        | inPlaceAlert = Just inPlaceAlert
                                      }
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

        NewsPageInfoMsg newsId pageInfoMsg ->
            case PageInfo.update pageInfoMsg of
                PageInfo.Success path json pageInfo ->
                    case decodeString newsInfoDecoder json of
                        Ok newsInfo ->
                            let
                                newsPageInfo =
                                    { pageInfo = pageInfo
                                    , newsInfo = newsInfo
                                    }
                            in
                                ( { model
                                    | newsPageInfos = Dict.insert newsId (Ok newsPageInfo) model.newsPageInfos
                                    , inPlaceAlert = Nothing
                                  }
                                , Cmd.none
                                , NoneOutMsg
                                )

                        Err error ->
                            let
                                info =
                                    "News Page Info: " ++ error
                            in
                                ( { model
                                    | inPlaceAlert = Nothing
                                    , newsPageInfos = Dict.insert newsId (Err info) model.newsPageInfos
                                  }
                                , Cmd.none
                                , AlertOutMsg AlertLevel.DangerLevel info
                                )

                PageInfo.FetchFail _ error ->
                    let
                        info =
                            "NewsId: " ++ newsId ++ " | Http Error: " ++ (Utils.toHumanReadable error)
                    in
                        ( { model
                            | inPlaceAlert = Nothing
                            , newsPageInfos = Dict.insert newsId (Err info) model.newsPageInfos
                          }
                        , Cmd.none
                        , AlertOutMsg AlertLevel.DangerLevel info
                        )

                PageInfo.BadJson _ _ error ->
                    let
                        info =
                            "News Page Info: " ++ error
                    in
                        ( { model
                            | inPlaceAlert = Nothing
                            , newsPageInfos = Dict.insert newsId (Err info) model.newsPageInfos
                          }
                        , Cmd.none
                        , AlertOutMsg AlertLevel.DangerLevel info
                        )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.h2 [] [ Html.text model.config.title ]
        , model.inPlaceAlert
            |> Maybe.map InPlaceAlert.view
            |> Maybe.withDefault (Html.text "")
        , MdlList.ul [] <|
            List.map
                (\newsId ->
                    MdlList.li []
                        (case Dict.get newsId model.newsPageInfos of
                            Just (Err errorInfo) ->
                                [ Html.span [ class "alert-danger" ] [ Html.text errorInfo ] ]

                            Just (Ok { pageInfo, newsInfo }) ->
                                let
                                    path =
                                        model.config.root ++ "/" ++ newsId
                                in
                                    (linkFromPageInfo path pageInfo)
                                        :: (newsInfo
                                                |> Maybe.map
                                                    (\{ author, date } ->
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
                model.newsIds
        ]
