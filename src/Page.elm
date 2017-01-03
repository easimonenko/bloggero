module Page exposing (Model, Msg(..), OutMsg(..), init, update, view)

import Debug
import Html
import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Navigation
import Task


-- Material Design Lite modules

import Material


-- Bloggero modules

import Alert.AlertLevel as AlertLevel
import Blog.BlogPage as BlogPage
import Blog.PostPage as PostPage
import Page.Breadcrumbs as Breadcrumbs
import Page.EmptyPlacePage as EmptyPlacePage
import Page.HomePage as HomePage
import Page.InPlaceAlertPage as InPlaceAlertPage
import Page.HtmlPage as HtmlPage
import Page.MarkdownPage as MarkdownPage
import Page.PageInfoRefresh as PageInfoRefreshPage
import Utils


--import Blog.PostPage


type alias Model =
    { mdl : Material.Model
    , location : Navigation.Location
    , pageInfo : Maybe PageInfo
    , breadcrumbs : Breadcrumbs.Model
    , driverModel : Driver
    }


type alias PageInfo =
    { title : String
    , driver : String
    }


type Driver
    = HomePage HomePage.Model
    | EmptyPlacePage EmptyPlacePage.Model
    | InPlaceAlertPage InPlaceAlertPage.Model
    | PageInfoRefreshPage PageInfoRefreshPage.Model
    | HtmlPage HtmlPage.Model
    | MarkdownPage MarkdownPage.Model
    | BlogPage BlogPage.Model
    | PostPage PostPage.Model


type Msg
    = Mdl (Material.Msg Msg)
    | PageInfoFetchSucceed String
    | PageInfoFetchFail Http.Error
    | PageContentFetchSucceed String
    | PageContentFetchFail Http.Error
    | PageInfoRefreshMsg PageInfoRefreshPage.Msg
    | HomePageMsg HomePage.Msg
    | EmptyPlacePageMsg EmptyPlacePage.Msg
    | InPlaceAlertPageMsg InPlaceAlertPage.Msg
    | HtmlPageMsg HtmlPage.Msg
    | MarkdownPageMsg MarkdownPage.Msg
    | BlogPageMsg BlogPage.Msg
    | PostPageMsg PostPage.Msg
    | BreadcrumbsMsg Breadcrumbs.Msg


type OutMsg
    = NoneOutMsg
    | AlertOutMsg AlertLevel.Level String


init : Navigation.Location -> ( Model, Cmd Msg, OutMsg )
init location =
    let
        ( emptyPlacePage, emptyPlacePageCmds ) =
            EmptyPlacePage.init location

        path =
            Utils.pagePath location

        ( breadcrumbs, breadcrumbsCmds ) =
            Breadcrumbs.init path
    in
        ( { mdl = Material.model
          , location = location
          , pageInfo = Nothing
          , breadcrumbs = breadcrumbs
          , driverModel = EmptyPlacePage emptyPlacePage
          }
        , Cmd.batch
            [ Task.attempt
                (\result ->
                    case result of
                        Err msg ->
                            PageInfoFetchFail msg

                        Ok msg ->
                            PageInfoFetchSucceed msg
                )
                (Http.toTask <| Http.getString <| path ++ "/index.json")
            , Cmd.map EmptyPlacePageMsg emptyPlacePageCmds
            , Cmd.map BreadcrumbsMsg breadcrumbsCmds
            ]
        , NoneOutMsg
        )


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        Mdl mdlMsg ->
            Utils.tuple2triple (Material.update mdlMsg model) NoneOutMsg

        PageInfoFetchSucceed pageInfoJson ->
            let
                pageInfoDecoder =
                    decode PageInfo
                        |> required "title" string
                        |> optional "driver" string "markdown"
            in
                case decodeString pageInfoDecoder pageInfoJson of
                    Ok pageInfo ->
                        case pageInfo.driver of
                            "markdown" ->
                                let
                                    ( page, pageCmds ) =
                                        MarkdownPage.init model.location
                                in
                                    ( { model
                                        | pageInfo = Just pageInfo
                                        , driverModel = MarkdownPage page
                                      }
                                    , Cmd.map MarkdownPageMsg pageCmds
                                    , AlertOutMsg
                                        AlertLevel.InfoLevel
                                        "PageInfoFetchSucceed: driver = markdown"
                                    )

                            "html" ->
                                let
                                    ( page, pageCmds ) =
                                        HtmlPage.init model.location
                                in
                                    ( { model
                                        | pageInfo = Just pageInfo
                                        , driverModel = HtmlPage page
                                      }
                                    , Cmd.map HtmlPageMsg pageCmds
                                    , AlertOutMsg
                                        AlertLevel.InfoLevel
                                        "PageInfoFetchSucceed: driver = html"
                                    )

                            "home" ->
                                let
                                    ( homePage, homePageCmds, homePageOutMsg ) =
                                        let
                                            defaultConfig =
                                                HomePage.defaultConfig
                                        in
                                            HomePage.init { defaultConfig | title = pageInfo.title }
                                in
                                    ( { model
                                        | pageInfo = Just pageInfo
                                        , driverModel = HomePage homePage
                                      }
                                    , Cmd.map HomePageMsg homePageCmds
                                    , AlertOutMsg
                                        AlertLevel.InfoLevel
                                        "PageInfoFetchSucceed: driver = home"
                                    )

                            "blog" ->
                                let
                                    ( blogPage, blogPageCmds, blogPageOutMsg ) =
                                        let
                                            defaultConfig =
                                                BlogPage.defaultConfig
                                        in
                                            BlogPage.init { defaultConfig | title = pageInfo.title }
                                in
                                    ( { model
                                        | pageInfo = Just pageInfo
                                        , driverModel = BlogPage blogPage
                                      }
                                    , Cmd.map BlogPageMsg blogPageCmds
                                    , AlertOutMsg
                                        AlertLevel.InfoLevel
                                        "PageInfoFetchSucceed: driver = blog"
                                    )

                            "post" ->
                                let
                                    ( page, pageCmds ) =
                                        PostPage.init model.location
                                in
                                    ( { model
                                        | pageInfo = Just pageInfo
                                        , driverModel = PostPage page
                                      }
                                    , Cmd.map PostPageMsg pageCmds
                                    , AlertOutMsg
                                        AlertLevel.InfoLevel
                                        "PageInfoFetchSucceed: driver = post"
                                    )

                            unknownType ->
                                let
                                    ( inPlaceAlertPage, inPlaceAlertPageCmds ) =
                                        InPlaceAlertPage.init
                                            AlertLevel.WarningLevel
                                            ("Unknown type of page: " ++ unknownType)
                                in
                                    ( { model
                                        | pageInfo = Just pageInfo
                                        , driverModel = InPlaceAlertPage inPlaceAlertPage
                                      }
                                    , Cmd.map InPlaceAlertPageMsg inPlaceAlertPageCmds
                                    , AlertOutMsg
                                        AlertLevel.WarningLevel
                                        ("Page info fetch succeed, but unknown content type: "
                                            ++ unknownType
                                        )
                                    )

                    Err info ->
                        let
                            ( inPlaceAlertPage, inPlaceAlertPageCmds ) =
                                InPlaceAlertPage.init
                                    AlertLevel.DangerLevel
                                    ((Utils.pagePath model.location) ++ ": " ++ info)
                        in
                            ( { model
                                | pageInfo = Nothing
                                , driverModel = InPlaceAlertPage inPlaceAlertPage
                              }
                            , Cmd.map InPlaceAlertPageMsg inPlaceAlertPageCmds
                            , AlertOutMsg
                                AlertLevel.DangerLevel
                                ("Page info fetch succeed, but parsed fail: " ++ info)
                            )

        PageInfoFetchFail (Http.NetworkError) ->
            let
                ( page, cmds, outMsg ) =
                    PageInfoRefreshPage.init
                        "Network Error"
                        "Network error: try refreshing the page later."
            in
                ( { model | driverModel = PageInfoRefreshPage page }
                , Cmd.map PageInfoRefreshMsg cmds
                , NoneOutMsg
                )

        PageInfoFetchFail (Http.Timeout) ->
            let
                ( page, cmds, outMsg ) =
                    PageInfoRefreshPage.init
                        "Http Timeout"
                        "Http timeout: try refreshing the page later."
            in
                ( { model | driverModel = PageInfoRefreshPage page }
                , Cmd.map PageInfoRefreshMsg cmds
                , NoneOutMsg
                )

        PageInfoRefreshMsg pageInfoRefreshMsg ->
            case model.driverModel of
                PageInfoRefreshPage pageModel ->
                    let
                        ( page, pageInfoRefreshCmds, outMsg ) =
                            PageInfoRefreshPage.update pageInfoRefreshMsg pageModel

                        pageInfoFetchCmds =
                            case outMsg of
                                PageInfoRefreshPage.PageInfoRefresh ->
                                    Task.attempt
                                        (\result ->
                                            case result of
                                                Err msg ->
                                                    PageInfoFetchFail msg

                                                Ok msg ->
                                                    PageInfoFetchSucceed msg
                                        )
                                        (Http.toTask <|
                                            Http.getString <|
                                                (Utils.pagePath model.location)
                                                    ++ "/index.json"
                                        )

                                _ ->
                                    Cmd.none
                    in
                        ( { model | driverModel = PageInfoRefreshPage page }
                        , Cmd.batch
                            [ Cmd.map PageInfoRefreshMsg pageInfoRefreshCmds
                            , pageInfoFetchCmds
                            ]
                        , NoneOutMsg
                        )

                _ ->
                    ( model, Cmd.none, NoneOutMsg )

        BreadcrumbsMsg breadcrumbsMsg ->
            let
                ( breadcrumbs, breadcrumbsCmds ) =
                    Breadcrumbs.update breadcrumbsMsg model.breadcrumbs
            in
                ( { model | breadcrumbs = breadcrumbs }
                , Cmd.map BreadcrumbsMsg breadcrumbsCmds
                , NoneOutMsg
                )

        HomePageMsg homePageMsg ->
            case model.driverModel of
                HomePage homePage ->
                    let
                        ( homePageNext, homePageCmds, homePageOutMsg ) =
                            HomePage.update homePageMsg homePage
                    in
                        ( { model | driverModel = HomePage homePageNext }
                        , Cmd.map HomePageMsg homePageCmds
                        , NoneOutMsg
                        )

                _ ->
                    ( model, Cmd.none, NoneOutMsg )

        BlogPageMsg blogPageMsg ->
            case model.driverModel of
                BlogPage blogPage ->
                    let
                        ( blogPageNext, blogPageCmds, blogPageOutMsg ) =
                            BlogPage.update blogPageMsg blogPage
                    in
                        ( { model | driverModel = BlogPage blogPageNext }
                        , Cmd.map BlogPageMsg blogPageCmds
                        , NoneOutMsg
                        )

                _ ->
                    ( model, Cmd.none, NoneOutMsg )

        PostPageMsg postPageMsg ->
            case model.driverModel of
                PostPage page ->
                    let
                        ( pageUpdated, cmds ) =
                            PostPage.update postPageMsg page
                    in
                        ( { model | driverModel = PostPage pageUpdated }
                        , Cmd.map PostPageMsg cmds
                        , NoneOutMsg
                        )

                _ ->
                    ( model, Cmd.none, NoneOutMsg )

        InPlaceAlertPageMsg inPlaceAlertPageMsg ->
            case model.driverModel of
                InPlaceAlertPage inPlaceAlertPage ->
                    let
                        ( inPlaceAlertPageUpdated, inPlaceAlertPageCmds ) =
                            InPlaceAlertPage.update inPlaceAlertPageMsg inPlaceAlertPage
                    in
                        ( { model | driverModel = InPlaceAlertPage inPlaceAlertPageUpdated }
                        , Cmd.map InPlaceAlertPageMsg inPlaceAlertPageCmds
                        , NoneOutMsg
                        )

                _ ->
                    ( model, Cmd.none, NoneOutMsg )

        HtmlPageMsg htmlPageMsg ->
            case model.driverModel of
                HtmlPage page ->
                    let
                        ( updatedPage, pageCmds ) =
                            HtmlPage.update htmlPageMsg page
                    in
                        ( { model | driverModel = HtmlPage updatedPage }
                        , Cmd.map HtmlPageMsg pageCmds
                        , NoneOutMsg
                        )

                _ ->
                    ( model, Cmd.none, NoneOutMsg )

        MarkdownPageMsg markdownPageMsg ->
            case model.driverModel of
                MarkdownPage page ->
                    let
                        ( updatedPage, pageCmds ) =
                            MarkdownPage.update markdownPageMsg page
                    in
                        ( { model | driverModel = MarkdownPage updatedPage }
                        , Cmd.map MarkdownPageMsg pageCmds
                        , NoneOutMsg
                        )

                _ ->
                    ( model, Cmd.none, NoneOutMsg )

        _ ->
            ( model, Cmd.none, NoneOutMsg )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.map BreadcrumbsMsg (Breadcrumbs.view model.breadcrumbs)
        , case model.driverModel of
            HomePage page ->
                Debug.log "HomePage"
                    Html.map
                    HomePageMsg
                    (HomePage.view page)

            EmptyPlacePage page ->
                Debug.log "EmptyPlacePage"
                    Html.map
                    EmptyPlacePageMsg
                    (EmptyPlacePage.view page)

            InPlaceAlertPage page ->
                Debug.log "InPlaceAlertPage"
                    Html.map
                    InPlaceAlertPageMsg
                    (InPlaceAlertPage.view page)

            PageInfoRefreshPage page ->
                Debug.log "PageInfoRefreshPage"
                    Html.map
                    PageInfoRefreshMsg
                    (PageInfoRefreshPage.view page)

            HtmlPage page ->
                Debug.log "HtmlPage"
                    Html.map
                    HtmlPageMsg
                    (HtmlPage.view page)

            MarkdownPage page ->
                Debug.log "MarkdownPage"
                    Html.map
                    MarkdownPageMsg
                    (MarkdownPage.view page)

            BlogPage page ->
                Debug.log "BlogPage"
                    Html.map
                    BlogPageMsg
                    (BlogPage.view page)

            PostPage page ->
                Debug.log "PostPage"
                    Html.map
                    PostPageMsg
                    (PostPage.view page)
        ]
