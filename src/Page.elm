module Page exposing (Model, Msg(..), OutMsg(..), init, update, view)

import Debug
import Html exposing (..)
import Html.App
import Html.Attributes exposing (class, style)
import Http
import Json.Decode exposing (..)
import Markdown
import Task
import VirtualDom


-- Material Design Lite modules

import Material
import Material.Button as Button
import Material.Color as Color
import Material.Options as Options


-- Bloggero modules

import Alert.AlertLevel as AlertLevel
import Page.EmptyPlacePage as EmptyPlacePage
import Page.HomePage as HomePage
import Page.InPlaceAlertPage as InPlaceAlertPage


--import Blog.PostPage


type alias Model =
    { mdl : Material.Model
    , path : String
    , query : String
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


type Msg
    = Mdl (Material.Msg Msg)
    | PageInfoFetchSucceed String
    | PageInfoFetchFail { path : String, query : String } Http.Error
    | PageContentFetchSucceed String
    | PageContentFetchFail Http.Error
    | ButtonPageInfoRefresh { path : String, query : String }
    | HomePageMsg HomePage.Msg
    | EmptyPlacePageMsg EmptyPlacePage.Msg
    | InPlaceAlertPageMsg InPlaceAlertPage.Msg


type OutMsg
    = NoneOutMsg
    | AlertOutMsg AlertLevel.Level String


init : String -> String -> ( Model, Cmd Msg, OutMsg )
init path query =
    let
        ( emptyPlacePage, emptyPlacePageCmds ) =
            EmptyPlacePage.init
    in
        ( { mdl = Material.model
          , path = path
          , query = query
          , title = ""
          , contentType = ""
          , contentFile = ""
          , content = text ""
          , pageDriverModel = EmptyPlacePage emptyPlacePage
          }
        , Cmd.batch
            [ Task.perform (PageInfoFetchFail { path = path, query = query })
                PageInfoFetchSucceed
                (Http.getString <| path ++ "/index.json")
            , Cmd.map EmptyPlacePageMsg emptyPlacePageCmds
            ]
        , NoneOutMsg
        )


tuple2triple : ( a, b ) -> c -> ( a, b, c )
tuple2triple t v =
    ( fst t, snd t, v )


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        Mdl mdlMsg ->
            tuple2triple (Material.update mdlMsg model) NoneOutMsg

        PageInfoFetchSucceed pageInfo ->
            let
                ( model, cmds, outMsg ) =
                    case decodeString ("title" := string) pageInfo of
                        Ok pageTitle ->
                            let
                                contentDecoder =
                                    object2 (,) (maybe ("type" := string)) (maybe ("file" := string))
                            in
                                case (decodeString <| "content" := contentDecoder) pageInfo of
                                    Ok ( ct, cf ) ->
                                        let
                                            contentType =
                                                Maybe.withDefault
                                                    "markdown"
                                                    ct

                                            contentFile =
                                                Maybe.withDefault "index.markdown" cf

                                            model =
                                                { model
                                                    | title = pageTitle
                                                    , contentType = contentType
                                                    , contentFile = contentFile
                                                }
                                        in
                                            case contentType of
                                                "markdown" ->
                                                    ( model
                                                    , Task.perform PageContentFetchFail
                                                        PageContentFetchSucceed
                                                        (Http.getString <| model.path ++ "/" ++ contentFile)
                                                    , AlertOutMsg
                                                        AlertLevel.InfoLevel
                                                        "PageInfoFetchSucceed: contentType = markdown"
                                                    )

                                                "html" ->
                                                    ( model
                                                    , Task.perform PageContentFetchFail
                                                        PageContentFetchSucceed
                                                        (Http.getString <| model.path ++ "/" ++ contentFile)
                                                    , AlertOutMsg
                                                        AlertLevel.InfoLevel
                                                        "PageInfoFetchSucceed: contentType = html"
                                                    )

                                                "home" ->
                                                    let
                                                        ( homePage, homePageCmds, homePageOutMsg ) =
                                                            HomePage.init { root = "/home", blogRoot = "/blog" }
                                                    in
                                                        ( { model | pageDriverModel = HomePage homePage }
                                                        , Cmd.map HomePageMsg homePageCmds
                                                        , AlertOutMsg
                                                            AlertLevel.InfoLevel
                                                            "PageInfoFetchSucceed: contentType = home"
                                                        )

                                                unknownType ->
                                                    let
                                                        ( inPlaceAlertPage, inPlaceAlertPageCmds ) =
                                                            InPlaceAlertPage.init
                                                                AlertLevel.WarningLevel
                                                                ("Unknown type of page: " ++ unknownType)
                                                    in
                                                        ( { model | pageDriverModel = InPlaceAlertPage inPlaceAlertPage }
                                                        , Cmd.map InPlaceAlertPageMsg inPlaceAlertPageCmds
                                                        , AlertOutMsg
                                                            AlertLevel.WarningLevel
                                                            ("Page info fetch succeed, but unknown content type: " ++ unknownType)
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

                        Err info ->
                            let
                                ( inPlaceAlertPage, inPlaceAlertPageCmds ) =
                                    InPlaceAlertPage.init AlertLevel.DangerLevel info
                            in
                                ( { model | pageDriverModel = InPlaceAlertPage inPlaceAlertPage }
                                , Cmd.map InPlaceAlertPageMsg inPlaceAlertPageCmds
                                , AlertOutMsg
                                    AlertLevel.WarningLevel
                                    ("Page info fetch succeed, but parsed fail: " ++ info)
                                )
            in
                ( model, cmds, outMsg )

        PageInfoFetchFail pageUrl (Http.NetworkError) ->
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

        PageContentFetchSucceed pageContent ->
            ( { model | content = article [] [ Markdown.toHtml [] pageContent ] }
            , Cmd.none
            , NoneOutMsg
            )

        PageContentFetchFail error ->
            ( { model | content = div [] [ text "The content of the page was not loaded." ] }
            , Cmd.none
            , NoneOutMsg
            )

        ButtonPageInfoRefresh pageUrl ->
            ( model
            , Task.perform (PageInfoFetchFail pageUrl)
                PageInfoFetchSucceed
                (Http.getString <| pageUrl.path ++ "/index.json")
            , NoneOutMsg
            )

        HomePageMsg homePageMsg ->
            case model.pageDriverModel of
                HomePage homePage ->
                    let
                        ( homePage, homePageCmds, homePageOutMsg ) =
                            HomePage.update homePageMsg homePage
                    in
                        ( { model | pageDriverModel = HomePage homePage }
                        , Cmd.map HomePageMsg homePageCmds
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

        _ ->
            ( model, Cmd.none, NoneOutMsg )


view : Model -> Html Msg
view model =
    case model.pageDriverModel of
        HomePage page ->
            Debug.log "HomePage"
                Html.App.map
                HomePageMsg
                (HomePage.view page)

        EmptyPlacePage page ->
            Debug.log "EmptyPlacePage"
                Html.App.map
                EmptyPlacePageMsg
                (EmptyPlacePage.view page)

        InPlaceAlertPage page ->
            Debug.log "InPlaceAlertPage"
                Html.App.map
                InPlaceAlertPageMsg
                (InPlaceAlertPage.view page)
