module Page exposing (Model, Msg(..), OutMsg(..), init, update, view)

import Html
import Html.Attributes exposing (class)
import Http
import Navigation


-- Material Design Lite modules

import Material


-- Bloggero modules

import Alert.AlertLevel as AlertLevel
import Blog.BlogPage as BlogPage
import Blog.PostPage as PostPage
import Page.PageInfo as PageInfo
import Page.Breadcrumbs as Breadcrumbs
import Page.HomePage as HomePage
import Page.InPlaceAlertPage as InPlaceAlertPage
import Page.HtmlPage as HtmlPage
import Page.MarkdownPage as MarkdownPage
import Page.PageInfoRefresh as PageInfoRefreshPage
import Utils


type alias Model =
    { mdl : Material.Model
    , location : Navigation.Location
    , pageInfo : Maybe PageInfo.PageInfo
    , breadcrumbs : Breadcrumbs.Model
    , driverModel : Driver
    }


type Driver
    = HomePage HomePage.Model
    | InPlaceAlertPage InPlaceAlertPage.Model
    | PageInfoRefreshPage PageInfoRefreshPage.Model
    | HtmlPage HtmlPage.Model
    | MarkdownPage MarkdownPage.Model
    | BlogPage BlogPage.Model
    | PostPage PostPage.Model


type Msg
    = Mdl (Material.Msg Msg)
    | PageInfoMsg PageInfo.Msg
    | PageInfoRefreshMsg PageInfoRefreshPage.Msg
    | HomePageMsg HomePage.Msg
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
        path =
            Utils.pagePath location

        ( inPlaceAlertPage, inPlaceAlertPageCmds ) =
            InPlaceAlertPage.init
                AlertLevel.InfoLevel
                ("Loading of page " ++ path ++ " ...")

        ( breadcrumbs, breadcrumbsCmds ) =
            Breadcrumbs.init path
    in
        ( { mdl = Material.model
          , location = location
          , pageInfo = Nothing
          , breadcrumbs = breadcrumbs
          , driverModel = InPlaceAlertPage inPlaceAlertPage
          }
        , Cmd.batch
            [ Cmd.map PageInfoMsg (PageInfo.init path)
            , Cmd.map InPlaceAlertPageMsg inPlaceAlertPageCmds
            , Cmd.map BreadcrumbsMsg breadcrumbsCmds
            ]
        , NoneOutMsg
        )


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        Mdl mdlMsg ->
            Utils.tuple2triple (Material.update Mdl mdlMsg model) NoneOutMsg

        PageInfoMsg pageInfoMsg ->
            case PageInfo.update pageInfoMsg of
                PageInfo.Success path pageInfoJson pageInfo ->
                    case pageInfo.driver of
                        "markdown" ->
                            let
                                ( page, pageCmds ) =
                                    MarkdownPage.init model.location pageInfoJson
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

                PageInfo.FetchFail path httpError ->
                    case httpError of
                        Http.NetworkError ->
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

                        Http.Timeout ->
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

                        error ->
                            let
                                alertLevel =
                                    AlertLevel.DangerLevel

                                errorInfo =
                                    Utils.toHumanReadable error

                                ( inPlaceAlertPage, inPlaceAlertPageCmds ) =
                                    InPlaceAlertPage.init alertLevel errorInfo
                            in
                                ( { model | driverModel = InPlaceAlertPage inPlaceAlertPage }
                                , Cmd.map InPlaceAlertPageMsg inPlaceAlertPageCmds
                                , AlertOutMsg alertLevel errorInfo
                                )

                PageInfo.BadJson path pageInfoJson errorInfo ->
                    let
                        ( inPlaceAlertPage, inPlaceAlertPageCmds ) =
                            InPlaceAlertPage.init
                                AlertLevel.DangerLevel
                                (path ++ ": " ++ errorInfo)
                    in
                        ( { model
                            | pageInfo = Nothing
                            , driverModel = InPlaceAlertPage inPlaceAlertPage
                          }
                        , Cmd.map InPlaceAlertPageMsg inPlaceAlertPageCmds
                        , AlertOutMsg
                            AlertLevel.DangerLevel
                            ("Page info fetch succeed, but parsed fail: " ++ errorInfo)
                        )

        PageInfoRefreshMsg pageInfoRefreshMsg ->
            case model.driverModel of
                PageInfoRefreshPage pageModel ->
                    let
                        ( page, pageInfoRefreshCmds, outMsg ) =
                            PageInfoRefreshPage.update pageInfoRefreshMsg pageModel
                    in
                        case outMsg of
                            PageInfoRefreshPage.PageInfoRefresh ->
                                let
                                    path =
                                        Utils.pagePath model.location

                                    ( breadcrumbs, breadcrumbsCmds ) =
                                        Breadcrumbs.init path
                                in
                                    ( { model
                                        | breadcrumbs = breadcrumbs
                                        , driverModel = PageInfoRefreshPage page
                                      }
                                    , Cmd.batch
                                        [ Cmd.map PageInfoRefreshMsg pageInfoRefreshCmds
                                        , Cmd.map PageInfoMsg (PageInfo.init path)
                                        , Cmd.map BreadcrumbsMsg breadcrumbsCmds
                                        ]
                                    , NoneOutMsg
                                    )

                            _ ->
                                ( model, Cmd.none, NoneOutMsg )

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
                        ( updatedPage, pageCmds, outMsg ) =
                            MarkdownPage.update markdownPageMsg page
                    in
                        ( { model | driverModel = MarkdownPage updatedPage }
                        , Cmd.map MarkdownPageMsg pageCmds
                        , case outMsg of
                            MarkdownPage.NoneOutMsg ->
                                NoneOutMsg

                            MarkdownPage.AlertOutMsg level info ->
                                AlertOutMsg level info
                        )

                _ ->
                    ( model, Cmd.none, NoneOutMsg )


view : Model -> Html.Html Msg
view model =
    Html.div [ class "page" ]
        [ if (Utils.pagePath model.location) /= "/home" && (model.pageInfo /= Nothing) then
            Html.map BreadcrumbsMsg (Breadcrumbs.view model.breadcrumbs)
          else
            Html.text ""
        , case model.driverModel of
            HomePage page ->
                Html.map
                    HomePageMsg
                    (HomePage.view page)

            InPlaceAlertPage page ->
                Html.map
                    InPlaceAlertPageMsg
                    (InPlaceAlertPage.view page)

            PageInfoRefreshPage page ->
                Html.map
                    PageInfoRefreshMsg
                    (PageInfoRefreshPage.view page)

            HtmlPage page ->
                Html.map
                    HtmlPageMsg
                    (HtmlPage.view page)

            MarkdownPage page ->
                Html.map
                    MarkdownPageMsg
                    (MarkdownPage.view page)

            BlogPage page ->
                Html.map
                    BlogPageMsg
                    (BlogPage.view page)

            PostPage page ->
                Html.map
                    PostPageMsg
                    (PostPage.view page)
        ]
