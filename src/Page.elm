module Page exposing (Model, Msg(..), OutMsg(..), init, update, view)

import Debug
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Http
import Json.Decode exposing (..)
import Markdown
import Navigation
import Task
import Tuple exposing (..)
import VirtualDom


-- Material Design Lite modules

import Material
import Material.Button as Button
import Material.Color as Color
import Material.Options as Options


-- Bloggero modules

import Alert.AlertLevel as AlertLevel
import Blog.BlogPage as BlogPage
import Page.EmptyPlacePage as EmptyPlacePage
import Page.HomePage as HomePage
import Page.InPlaceAlertPage as InPlaceAlertPage
import Page.HtmlPage as HtmlPage
import Page.MarkdownPage as MarkdownPage
import Utils


--import Blog.PostPage


type alias Model =
    { mdl : Material.Model
    , location : Navigation.Location
    , title : String
    , contentType : String
    , contentFile : String
    , content : VirtualDom.Node Msg
    , pageDriverModel : Driver
    }


type Driver
    = HomePage HomePage.Model
    | EmptyPlacePage EmptyPlacePage.Model
    | InPlaceAlertPage InPlaceAlertPage.Model
    | BlogPage BlogPage.Model
    | HtmlPage HtmlPage.Model
    | MarkdownPage MarkdownPage.Model


type Msg
    = Mdl (Material.Msg Msg)
    | PageInfoFetchSucceed String
    | PageInfoFetchFail Navigation.Location Http.Error
    | PageContentFetchSucceed String
    | PageContentFetchFail Http.Error
    | ButtonPageInfoRefresh Navigation.Location
    | HomePageMsg HomePage.Msg
    | EmptyPlacePageMsg EmptyPlacePage.Msg
    | InPlaceAlertPageMsg InPlaceAlertPage.Msg
    | BlogPageMsg BlogPage.Msg
    | HtmlPageMsg HtmlPage.Msg
    | MarkdownPageMsg MarkdownPage.Msg


type OutMsg
    = NoneOutMsg
    | AlertOutMsg AlertLevel.Level String


init : Navigation.Location -> ( Model, Cmd Msg, OutMsg )
init location =
    let
        ( emptyPlacePage, emptyPlacePageCmds ) =
            EmptyPlacePage.init location
    in
        ( { mdl = Material.model
          , location = location
          , title = ""
          , contentType = ""
          , contentFile = ""
          , content = text ""
          , pageDriverModel = EmptyPlacePage emptyPlacePage
          }
        , Cmd.batch
            [ Task.attempt
                (\result ->
                    case result of
                        Err msg ->
                            PageInfoFetchFail location msg

                        Ok msg ->
                            PageInfoFetchSucceed msg
                )
                (Http.toTask <| Http.getString <| (Utils.pagePath location) ++ "/index.json")
            , Cmd.map EmptyPlacePageMsg emptyPlacePageCmds
            ]
        , NoneOutMsg
        )


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        Mdl mdlMsg ->
            Utils.tuple2triple (Material.update mdlMsg model) NoneOutMsg

        PageInfoFetchSucceed pageInfo ->
            let
                pageInfoDecoder =
                    (map2 (,) (field "title" string) (maybe (field "type" string)))
            in
                case decodeString pageInfoDecoder pageInfo of
                    Ok ( pageTitle, contentType ) ->
                        case Maybe.withDefault "markdown" contentType of
                            "markdown" ->
                                let
                                    ( page, pageCmds ) =
                                        MarkdownPage.init model.location
                                in
                                    ( { model
                                        | title = pageTitle
                                        , contentType = "markdown"
                                        , pageDriverModel = MarkdownPage page
                                      }
                                    , Cmd.map MarkdownPageMsg pageCmds
                                    , AlertOutMsg
                                        AlertLevel.InfoLevel
                                        "PageInfoFetchSucceed: contentType = markdown"
                                    )

                            "html" ->
                                let
                                    ( page, pageCmds ) =
                                        HtmlPage.init model.location
                                in
                                    ( { model
                                        | title = pageTitle
                                        , contentType = "html"
                                        , pageDriverModel = HtmlPage page
                                      }
                                    , Cmd.map HtmlPageMsg pageCmds
                                    , AlertOutMsg
                                        AlertLevel.InfoLevel
                                        "PageInfoFetchSucceed: contentType = html"
                                    )

                            "home" ->
                                let
                                    ( homePage, homePageCmds, homePageOutMsg ) =
                                        let
                                            defaultConfig =
                                                HomePage.defaultConfig
                                        in
                                            HomePage.init { defaultConfig | title = pageTitle }
                                in
                                    ( { model
                                        | title = pageTitle
                                        , contentType = "home"
                                        , pageDriverModel = HomePage homePage
                                      }
                                    , Cmd.map HomePageMsg homePageCmds
                                    , AlertOutMsg
                                        AlertLevel.InfoLevel
                                        "PageInfoFetchSucceed: contentType = home"
                                    )

                            "blog" ->
                                let
                                    ( blogPage, blogPageCmds, blogPageOutMsg ) =
                                        let
                                            defaultConfig =
                                                BlogPage.defaultConfig
                                        in
                                            BlogPage.init { defaultConfig | title = pageTitle }
                                in
                                    ( { model
                                        | title = pageTitle
                                        , contentType = "blog"
                                        , pageDriverModel = BlogPage blogPage
                                      }
                                    , Cmd.map BlogPageMsg blogPageCmds
                                    , AlertOutMsg
                                        AlertLevel.InfoLevel
                                        "PageInfoFetchSucceed: contentType = blog"
                                    )

                            unknownType ->
                                let
                                    ( inPlaceAlertPage, inPlaceAlertPageCmds ) =
                                        InPlaceAlertPage.init
                                            AlertLevel.WarningLevel
                                            ("Unknown type of page: " ++ unknownType)
                                in
                                    ( { model
                                        | title = pageTitle
                                        , pageDriverModel = InPlaceAlertPage inPlaceAlertPage
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
                                InPlaceAlertPage.init AlertLevel.DangerLevel info
                        in
                            ( { model | pageDriverModel = InPlaceAlertPage inPlaceAlertPage }
                            , Cmd.map InPlaceAlertPageMsg inPlaceAlertPageCmds
                            , AlertOutMsg
                                AlertLevel.DangerLevel
                                ("Page info fetch succeed, but parsed fail: " ++ info)
                            )

        PageInfoFetchFail location (Http.NetworkError) ->
            ( { model
                | title = "Network Error"
                , content =
                    Options.div [ Options.cs "mdl-card mdl-shadow--2dp" ]
                        [ Options.div [ Options.cs "mdl-card__title" ]
                            [ h1 [ class "mdl-card__title-text" ] [ text "Network Error" ]
                            ]
                        , Options.div
                            [ Options.cs "mdl-card__supporting-text"
                            , Color.background (Color.color Color.Yellow Color.S50)
                            ]
                            [ text "Network error: try refreshing the page later."
                            ]
                        , Options.div [ Options.cs "mdl-card__actions mdl-card--border" ]
                            [ Button.render Mdl
                                [ 0 ]
                                model.mdl
                                [ Button.raised
                                , Button.colored
                                , Button.ripple
                                , Button.onClick (ButtonPageInfoRefresh location)
                                ]
                                [ text "Refresh"
                                ]
                            ]
                        ]
              }
            , Cmd.none
            , NoneOutMsg
            )

        PageInfoFetchFail pageUrl (Http.Timeout) ->
            ( { model
                | title = "Http Timeout"
                , content =
                    Options.div [ Options.cs "mdl-card mdl-shadow--2dp" ]
                        [ Options.div [ Options.cs "mdl-card__title" ]
                            [ h1 [ class "mdl-card__title-text" ] [ text "Http Timeout" ]
                            ]
                        , Options.div
                            [ Options.cs "mdl-card__supporting-text"
                            , Color.background (Color.color Color.Yellow Color.S50)
                            ]
                            [ text "Http timeout: try refreshing the page later."
                            ]
                        , Options.div [ Options.cs "mdl-card__actions mdl-card--border" ]
                            [ Button.render Mdl
                                [ 0 ]
                                model.mdl
                                [ Button.raised
                                , Button.colored
                                , Button.ripple
                                , Button.onClick (ButtonPageInfoRefresh pageUrl)
                                ]
                                [ text "Refresh"
                                ]
                            ]
                        ]
              }
            , Cmd.none
            , NoneOutMsg
            )

        ButtonPageInfoRefresh location ->
            ( model
            , Task.attempt
                (\result ->
                    case result of
                        Err msg ->
                            PageInfoFetchFail location msg

                        Ok msg ->
                            PageInfoFetchSucceed msg
                )
                (Http.toTask <| Http.getString <| location.hash ++ "/index.json")
            , NoneOutMsg
            )

        HomePageMsg homePageMsg ->
            case model.pageDriverModel of
                HomePage homePage ->
                    let
                        ( homePageNext, homePageCmds, homePageOutMsg ) =
                            HomePage.update homePageMsg homePage
                    in
                        ( { model | pageDriverModel = HomePage homePageNext }
                        , Cmd.map HomePageMsg homePageCmds
                        , NoneOutMsg
                        )

                _ ->
                    ( model, Cmd.none, NoneOutMsg )

        BlogPageMsg blogPageMsg ->
            case model.pageDriverModel of
                BlogPage blogPage ->
                    let
                        ( blogPageNext, blogPageCmds, blogPageOutMsg ) =
                            BlogPage.update blogPageMsg blogPage
                    in
                        ( { model | pageDriverModel = BlogPage blogPageNext }
                        , Cmd.map BlogPageMsg blogPageCmds
                        , NoneOutMsg
                        )

                _ ->
                    ( model, Cmd.none, NoneOutMsg )

        InPlaceAlertPageMsg inPlaceAlertPageMsg ->
            case model.pageDriverModel of
                InPlaceAlertPage inPlaceAlertPage ->
                    let
                        ( inPlaceAlertPageUpdated, inPlaceAlertPageCmds ) =
                            InPlaceAlertPage.update inPlaceAlertPageMsg inPlaceAlertPage
                    in
                        ( { model | pageDriverModel = InPlaceAlertPage inPlaceAlertPageUpdated }
                        , Cmd.map InPlaceAlertPageMsg inPlaceAlertPageCmds
                        , NoneOutMsg
                        )

                _ ->
                    ( model, Cmd.none, NoneOutMsg )

        HtmlPageMsg htmlPageMsg ->
            case model.pageDriverModel of
                HtmlPage page ->
                    let
                        ( updatedPage, pageCmds ) =
                            HtmlPage.update htmlPageMsg page
                    in
                        ( { model | pageDriverModel = HtmlPage updatedPage }
                        , Cmd.map HtmlPageMsg pageCmds
                        , NoneOutMsg
                        )

                _ ->
                    ( model, Cmd.none, NoneOutMsg )

        MarkdownPageMsg markdownPageMsg ->
            case model.pageDriverModel of
                MarkdownPage page ->
                    let
                        ( updatedPage, pageCmds ) =
                            MarkdownPage.update markdownPageMsg page
                    in
                        ( { model | pageDriverModel = MarkdownPage updatedPage }
                        , Cmd.map MarkdownPageMsg pageCmds
                        , NoneOutMsg
                        )

                _ ->
                    ( model, Cmd.none, NoneOutMsg )

        _ ->
            ( model, Cmd.none, NoneOutMsg )


view : Model -> Html Msg
view model =
    case model.pageDriverModel of
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

        BlogPage page ->
            Debug.log "BlogPage"
                Html.map
                BlogPageMsg
                (BlogPage.view page)

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
