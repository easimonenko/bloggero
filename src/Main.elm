port module Main exposing (main)

import Html exposing (..)
import Html.App
import Html.Attributes exposing (class, classList, href, property, style)
import Html.Attributes.Extra exposing (innerHtml)
import Http
import Json.Decode exposing ((:=), decodeString, list, maybe, object5, string)
import Json.Encode
import List exposing (filter, head, map, member, tail)
import Maybe exposing (withDefault)
import String exposing (split)
import Task
import List.Extra exposing (find)
import Material
import Material.Button as Button
import Material.Footer as Footer
import Material.Icon as Icon
import Material.Layout as Layout
import Material.Snackbar as Snackbar
import Navigation
import Alert
import Page


main : Program Never
main =
    Navigation.program (Navigation.makeParser urlParser)
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        , urlUpdate = urlUpdate
        }


type alias Model =
    { mdl : Material.Model
    , alert : Alert.Model
    , page : Maybe Page.Model
    , snackbar : Snackbar.Model ()
    , config :
        { title : String
        , root : String
        , mode : Mode
        , sections : List SectionItem
        }
    , isConfigLoaded : Maybe Bool
    , debugMessages : List String
    , sectionId : Maybe String
    }


type Mode
    = DevelopmentMode
    | ProductionMode
    | UnknownMode


type alias SectionItem =
    { id : String
    , title : String
    , route : String
    , icon : Maybe String
    , placement : List Placement
    }


type Placement
    = HeaderPlacement
    | DrawerPlacement
    | FooterPlacement
    | SiteMapPlacement
    | UnknownPlacement


type Msg
    = Mdl (Material.Msg Msg)
    | ConfigFetchSucceed { path : String, query : String } String
    | ConfigFetchFail Http.Error
    | SnackbarMsg (Snackbar.Msg ())
    | AlertMsg Alert.Msg
    | PageMsg Page.Msg
    | HideDrawer


port title : String -> Cmd msg


init : Result String { path : String, query : String } -> ( Model, Cmd Msg )
init result =
    let
        ( alert, alertCmds ) =
            Alert.init Alert.InfoLevel "The application is downloaded. Please wait a bit."

        model =
            { mdl = Material.model
            , snackbar = Snackbar.model
            , alert = alert
            , page = Nothing
            , config =
                { title = ""
                , root = ""
                , mode = DevelopmentMode
                , sections = []
                }
            , isConfigLoaded = Nothing
            , debugMessages = []
            , sectionId = Nothing
            }

        loadConfig parsedUrl =
            Task.perform ConfigFetchFail (ConfigFetchSucceed parsedUrl) (Http.getString "/config.json")
    in
        case result of
            Err info ->
                let
                    ( snackbar', snackbarCmds ) =
                        Snackbar.add (Snackbar.toast () <| "Parsing URL: " ++ info) model.snackbar
                in
                    ( { model | snackbar = snackbar' }
                    , Cmd.batch
                        [ Layout.sub0 Mdl
                        , Cmd.map SnackbarMsg snackbarCmds
                        , Cmd.map AlertMsg alertCmds
                        , loadConfig { path = "/error/unknown-url", query = "" }
                        ]
                    )

            Ok pageUrl ->
                if pageUrl.path == "" then
                    let
                        ( snackbar', snackbarCmds ) =
                            Snackbar.add (Snackbar.toast () <| "Redirect to /home.") model.snackbar
                    in
                        ( { model | snackbar = snackbar' }
                        , Cmd.batch
                            [ Layout.sub0 Mdl
                            , Cmd.map SnackbarMsg snackbarCmds
                            , Cmd.map AlertMsg alertCmds
                            , loadConfig { path = "/home", query = pageUrl.query }
                            ]
                        )
                else
                    ( model
                    , Cmd.batch [ Layout.sub0 Mdl, loadConfig pageUrl, Cmd.map AlertMsg alertCmds ]
                    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl mdlMsg ->
            Material.update mdlMsg model

        ConfigFetchFail httpError ->
            let
                message =
                    "The config is not loaded "
                        ++ case httpError of
                            Http.Timeout ->
                                "[Timeout]."

                            Http.NetworkError ->
                                "[NetworkError]."

                            Http.UnexpectedPayload info ->
                                "[UnexpectedPayload]: " ++ info

                            Http.BadResponse code info ->
                                "[BadResponse]: " ++ (toString code) ++ " - " ++ info

                ( snackbar, snackbarCmds ) =
                    Snackbar.add (Snackbar.toast () message)
                        model.snackbar

                ( alert, alertCmds ) =
                    Alert.init Alert.DangerLevel message
            in
                ( { model | snackbar = snackbar, alert = alert, isConfigLoaded = Just False }
                , Cmd.batch
                    [ Cmd.map AlertMsg alertCmds
                    , Cmd.map SnackbarMsg snackbarCmds
                    ]
                )

        ConfigFetchSucceed pageUrl config ->
            let
                blogTitle =
                    decodeString ("title" := string) config

                blogRoot =
                    decodeString ("root" := string) config

                blogMode =
                    decodeString
                        ("mode"
                            := Json.Decode.map
                                (\item ->
                                    case item of
                                        "development" ->
                                            DevelopmentMode

                                        "production" ->
                                            ProductionMode

                                        _ ->
                                            UnknownMode
                                )
                                string
                        )
                        config

                placementItemListDecoder =
                    list
                        (Json.Decode.map
                            (\item ->
                                case item of
                                    "header" ->
                                        HeaderPlacement

                                    "drawer" ->
                                        DrawerPlacement

                                    "footer" ->
                                        FooterPlacement

                                    "sitemap" ->
                                        SiteMapPlacement

                                    _ ->
                                        UnknownPlacement
                            )
                            string
                        )

                sectionItemListDecoder =
                    list
                        (object5 SectionItem
                            ("id" := string)
                            ("title" := string)
                            ("route" := string)
                            (maybe ("icon" := string))
                            ("placement" := placementItemListDecoder)
                        )

                blogSections =
                    decodeString ("sections" := sectionItemListDecoder) config
            in
                case Result.map4 (,,,) blogTitle blogRoot blogMode blogSections of
                    Ok ( title', root', mode', sections' ) ->
                        let
                            ( snackbar', snackbarCmds ) =
                                Snackbar.add (Snackbar.toast () "The config is loaded.") model.snackbar
                        in
                            ( { model
                                | snackbar = snackbar'
                                , config =
                                    let
                                        modelConfig =
                                            model.config
                                    in
                                        { modelConfig
                                            | title = title'
                                            , root = root'
                                            , mode = mode'
                                            , sections = sections'
                                        }
                                , isConfigLoaded = Just True
                              }
                            , Cmd.batch
                                [ title title'
                                , Navigation.modifyUrl <| "/#!" ++ pageUrl.path
                                , Cmd.map SnackbarMsg snackbarCmds
                                ]
                            )

                    Err info ->
                        let
                            message =
                                "The config is not loaded. " ++ info

                            ( snackbar', snackbarCmds ) =
                                Snackbar.add (Snackbar.toast () message) model.snackbar

                            ( alert, alertCmds ) =
                                Alert.init Alert.DangerLevel message
                        in
                            ( { model
                                | snackbar = snackbar'
                                , alert = alert
                                , isConfigLoaded = Just False
                              }
                            , Cmd.batch [ Cmd.map SnackbarMsg snackbarCmds, Cmd.map AlertMsg alertCmds ]
                            )

        AlertMsg alertMsg ->
            let
                ( updatedAlert, alertCmds ) =
                    Alert.update alertMsg model.alert
            in
                ( { model | alert = updatedAlert }, Cmd.map AlertMsg alertCmds )

        PageMsg pageMsg ->
            case pageMsg of
                Page.PageInfoFetchFail pageUrl (Http.BadResponse statusCode statusInfo) ->
                    let
                        ( snackbar', snackbarCmds ) =
                            Snackbar.add (Snackbar.toast () <| "Bad response: " ++ (toString statusCode) ++ " - " ++ statusInfo)
                                model.snackbar
                    in
                        ( { model | snackbar = snackbar' }
                        , Cmd.batch
                            [ Cmd.map SnackbarMsg snackbarCmds
                            , Navigation.modifyUrl <| "/#!/error/bad-response/" ++ toString statusCode
                            ]
                        )

                Page.PageInfoFetchFail pageUrl (Http.Timeout) ->
                    let
                        ( snackbar', snackbarCmds ) =
                            Snackbar.add (Snackbar.toast () "Http Timeout.")
                                model.snackbar
                    in
                        case model.page of
                            Just page ->
                                let
                                    ( updatedPage, pageCmds ) =
                                        Page.update pageMsg page
                                in
                                    ( { model | page = Just updatedPage, snackbar = snackbar' }
                                    , Cmd.batch
                                        [ Cmd.map SnackbarMsg snackbarCmds
                                        , Cmd.map PageMsg pageCmds
                                        ]
                                    )

                            Nothing ->
                                let
                                    ( snackbar', snackbarCmds ) =
                                        Snackbar.add (Snackbar.toast () "WTF: page is Nothing!")
                                            model.snackbar
                                in
                                    ( { model | snackbar = snackbar' }
                                    , Cmd.map SnackbarMsg snackbarCmds
                                    )

                Page.PageInfoFetchFail pageUrl (Http.NetworkError) ->
                    let
                        ( snackbar', snackbarCmds ) =
                            Snackbar.add (Snackbar.toast () "Network error.")
                                model.snackbar
                    in
                        case model.page of
                            Just page ->
                                let
                                    ( updatedPage, pageCmds ) =
                                        Page.update pageMsg page
                                in
                                    ( { model | page = Just updatedPage, snackbar = snackbar' }
                                    , Cmd.batch
                                        [ Cmd.map SnackbarMsg snackbarCmds
                                        , Cmd.map PageMsg pageCmds
                                        ]
                                    )

                            Nothing ->
                                let
                                    ( snackbar', snackbarCmds ) =
                                        Snackbar.add (Snackbar.toast () "WTF: page is Nothing!")
                                            model.snackbar
                                in
                                    ( { model | snackbar = snackbar' }
                                    , Cmd.map SnackbarMsg snackbarCmds
                                    )

                Page.PageInfoFetchFail pageUrl (Http.UnexpectedPayload info) ->
                    let
                        ( snackbar', snackbarCmds ) =
                            Snackbar.add (Snackbar.toast () <| "Unexpected payload: " ++ info)
                                model.snackbar
                    in
                        ( { model | snackbar = snackbar' }
                        , Cmd.batch
                            [ Cmd.map SnackbarMsg snackbarCmds
                            , Navigation.modifyUrl <| "/#!/error/unexpected-payload"
                            ]
                        )

                Page.PageInfoFetchSucceed _ ->
                    case model.page of
                        Just page ->
                            let
                                ( updatedPage, pageCmds ) =
                                    Page.update pageMsg page
                            in
                                ( { model | page = Just updatedPage }
                                , Cmd.batch
                                    [ Cmd.map PageMsg pageCmds
                                    ]
                                )

                        Nothing ->
                            let
                                ( snackbar', snackbarCmds ) =
                                    Snackbar.add (Snackbar.toast () "WTF: page is Nothing!")
                                        model.snackbar
                            in
                                ( { model | snackbar = snackbar' }
                                , Cmd.map SnackbarMsg snackbarCmds
                                )

                _ ->
                    case model.page of
                        Just page ->
                            let
                                ( updatedPage, pageCmds ) =
                                    Page.update pageMsg page
                            in
                                ( { model | page = Just updatedPage }
                                , Cmd.map PageMsg pageCmds
                                )

                        Nothing ->
                            let
                                ( snackbar', snackbarCmds ) =
                                    Snackbar.add (Snackbar.toast () "WTF: page is Nothing!")
                                        model.snackbar
                            in
                                ( { model | snackbar = snackbar' }
                                , Cmd.map SnackbarMsg snackbarCmds
                                )

        SnackbarMsg snackbarMsg ->
            let
                ( snackbar', snackbarCmds ) =
                    Snackbar.update snackbarMsg model.snackbar
            in
                ( { model | snackbar = snackbar' }
                , Cmd.map SnackbarMsg snackbarCmds
                )

        HideDrawer ->
            ( model, Task.perform identity identity (Task.succeed (Layout.toggleDrawer Mdl)) )


subscriptions =
    .mdl >> Layout.subs Mdl


urlParser :
    Navigation.Location
    -> Result String { path : String, query : String }
urlParser location =
    Ok { path = String.dropLeft 2 location.hash, query = String.dropLeft 1 location.search }


urlUpdate : Result String { path : String, query : String } -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    case result of
        Err info ->
            let
                ( snackbar', snackbarCmds ) =
                    Snackbar.add (Snackbar.toast () <| "Unknown URL: " ++ info)
                        model.snackbar
            in
                ( { model | snackbar = snackbar' }
                , Cmd.batch
                    [ title <| model.config.title ++ " - Unknown URL"
                    , Cmd.map SnackbarMsg snackbarCmds
                    , Navigation.modifyUrl "/#!/error/unknown-url"
                    ]
                )

        Ok parsedUrl ->
            if parsedUrl.path == "" then
                let
                    ( snackbar', snackbarCmds ) =
                        Snackbar.add (Snackbar.toast () <| "Redirect to /home")
                            model.snackbar
                in
                    ( { model | snackbar = snackbar' }
                    , Cmd.batch
                        [ Cmd.map SnackbarMsg snackbarCmds
                        , Navigation.modifyUrl "/#!/home"
                        ]
                    )
            else
                let
                    ( page, pageFx ) =
                        Page.init parsedUrl.path parsedUrl.query model.config.root

                    section =
                        find
                            (\item ->
                                item.route
                                    == "/"
                                    ++ withDefault "" (head <| withDefault [] (tail (split "/" parsedUrl.path)))
                            )
                            model.config.sections
                in
                    ( { model
                        | page = Just page
                        , sectionId = Maybe.map .id section
                      }
                    , Cmd.batch
                        [ title
                            <| model.config.title
                            ++ case section of
                                Just s ->
                                    " - " ++ s.title

                                Nothing ->
                                    ""
                        , Cmd.map PageMsg pageFx
                        , if model.mdl.layout.isDrawerOpen then
                            Task.perform identity identity (Task.succeed HideDrawer)
                          else
                            Cmd.none
                        ]
                    )


headerView : Model -> List (Html Msg)
headerView model =
    let
        makeLink item =
            Layout.link [ Layout.href ("/#!" ++ item.route) ]
                [ (case item.icon of
                    Just iconName ->
                        Icon.i iconName

                    Nothing ->
                        span [] [ text "" ]
                  )
                , text item.title
                ]
    in
        [ div [ class "mdl-layout--large-screen-only" ]
            [ Layout.row []
                [ Layout.title []
                    (case model.isConfigLoaded of
                        Just True ->
                            case model.sectionId of
                                Just sectionId ->
                                    [ text model.config.title
                                    , span [ innerHtml "&nbsp;::&nbsp;" ] []
                                    , text
                                        (case find (\item -> item.id == sectionId) model.config.sections of
                                            Just section ->
                                                section.title

                                            Nothing ->
                                                ""
                                        )
                                    ]

                                Nothing ->
                                    [ text "" ]

                        _ ->
                            [ text "" ]
                    )
                , Layout.spacer
                , Layout.navigation []
                    (map makeLink
                        <| filter (\item -> member HeaderPlacement item.placement) model.config.sections
                    )
                ]
            ]
        , div [ class "mdl-layout--small-screen-only" ]
            [ Layout.row []
                [ Layout.title []
                    (case model.isConfigLoaded of
                        Just True ->
                            case model.sectionId of
                                Just sectionId ->
                                    [ text model.config.title
                                    , span [ innerHtml "&nbsp;::&nbsp;" ] []
                                    , text
                                        (case find (\item -> item.id == sectionId) model.config.sections of
                                            Just section ->
                                                section.title

                                            Nothing ->
                                                ""
                                        )
                                    ]

                                Nothing ->
                                    [ text "" ]

                        _ ->
                            [ text "" ]
                    )
                ]
            ]
        ]


drawerView : Model -> List (Html Msg)
drawerView model =
    let
        makeLink item =
            Layout.link [ Layout.href ("/#!" ++ item.route) ]
                [ (case item.icon of
                    Just iconName ->
                        Icon.i iconName

                    Nothing ->
                        span [] [ text "" ]
                  )
                , text item.title
                ]
    in
        [ Layout.title []
            [ case model.isConfigLoaded of
                Just True ->
                    case model.sectionId of
                        Just sectionId ->
                            span []
                                [ text model.config.title
                                , span [ innerHtml "&nbsp;::&nbsp;" ] []
                                , text
                                    (case find (\item -> item.id == sectionId) model.config.sections of
                                        Just section ->
                                            section.title

                                        Nothing ->
                                            ""
                                    )
                                ]

                        Nothing ->
                            text ""

                _ ->
                    text ""
            , Button.render Mdl
                [ 0 ]
                model.mdl
                [ Button.icon
                , Button.colored
                , Button.ripple
                , Button.onClick HideDrawer
                ]
                [ Icon.i "close"
                ]
            ]
        , hr [] []
        , Layout.navigation []
            (map makeLink
                <| filter (\item -> member DrawerPlacement item.placement) model.config.sections
            )
        ]


mainView : Model -> List (Html Msg)
mainView model =
    [ case model.isConfigLoaded of
        Just True ->
            case model.page of
                Just page ->
                    Html.App.map PageMsg (Page.view page)

                Nothing ->
                    Html.App.map AlertMsg (Alert.view model.alert)

        Just False ->
            Html.App.map AlertMsg (Alert.view model.alert)

        Nothing ->
            Html.App.map AlertMsg (Alert.view model.alert)
    , Layout.spacer
    , Footer.mini []
        { left =
            Footer.left []
                [ Footer.logo []
                    [ Footer.html <| span [ innerHtml "&copy; 2016" ] []
                    ]
                ]
        , right =
            Footer.right []
                [ Footer.html
                    <| a
                        [ href "https://github.com/easimonenko/bloggero-elm-mdl"
                        , Html.Attributes.property "role" (Json.Encode.string "button")
                        , Html.Attributes.title "GitHub"
                        ]
                        [ i
                            [ class "fa fa-github fa-3x fa-hover"
                            , property "aria-hidden" (Json.Encode.bool True)
                            ]
                            []
                        ]
                ]
        }
    , Snackbar.view model.snackbar |> Html.App.map SnackbarMsg
    ]


tabsView =
    ( [], [] )


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader
        , Layout.fixedTabs
        , Layout.seamed
        , Layout.waterfall True
        ]
        { header = headerView model
        , drawer = drawerView model
        , main = mainView model
        , tabs = tabsView
        }
