module News.NewsList exposing (Model, Msg, Config, defaultConfig, init, update, view)

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


type alias Model =
    { config : Config
    , newsIds : List NewsId
    , newsPageInfos : Dict.Dict NewsId NewsPageInfo
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


type alias Config =
    { root : String
    , title : String
    }


defaultConfig : Config
defaultConfig =
    { root = "/news"
    , title = "News List"
    }


init : Config -> ( Model, Cmd Msg )
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
        )


update : Msg -> Model -> ( Model, Cmd Msg )
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
                                            inPlaceAlert =
                                                InPlaceAlert.init
                                                    AlertLevel.WarningLevel
                                                    "News list is empty."
                                        in
                                            ( { model
                                                | inPlaceAlert = Just inPlaceAlert
                                              }
                                            , Cmd.none
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
                                    )

                _ ->
                    ( model, Cmd.none )

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
                                    | newsPageInfos = Dict.insert newsId newsPageInfo model.newsPageInfos
                                    , inPlaceAlert = Nothing
                                  }
                                , Cmd.none
                                )

                        Err error ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


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
                            Just { pageInfo, newsInfo } ->
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
