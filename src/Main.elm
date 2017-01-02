port module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (class, classList, href, property, style)
import Html.Attributes.Extra exposing (innerHtml)
import Http
import Json.Decode exposing (field, decodeString, list, maybe, map5, string)
import Json.Encode
import List
import List.Extra
import Maybe
import Navigation
import String
import Task


-- Material Design Lite modules

import Material
import Material.Button as Button
import Material.Elevation as Elevation
import Material.Grid as Grid exposing (Device(..))
import Material.Footer as Footer
import Material.Icon as Icon
import Material.Layout as Layout
import Material.Snackbar as Snackbar


-- Bloggero modules

import Alert.AlertList as AlertList
import Alert.AlertLevel as AlertLevel
import Page


type alias Model =
    { mdl : Material.Model
    , location : Navigation.Location
    , alertList : AlertList.Model
    , page : Maybe Page.Model
    , snackbar : Snackbar.Model ()
    , config : Maybe Config
    , sectionId : Maybe String
    }


type alias Config =
    { title : String
    , mode : Mode
    , sections : List SectionItem
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
    | LocationChange Navigation.Location
    | ConfigFetchSucceed Navigation.Location String
    | ConfigFetchFail Http.Error
    | SnackbarMsg (Snackbar.Msg ())
    | AlertListMsg AlertList.Msg
    | PageMsg Page.Msg
    | HideDrawer


port title : String -> Cmd msg


main : Program Never Model Msg
main =
    Navigation.program locationChangeHandler
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


locationChangeHandler : Navigation.Location -> Msg
locationChangeHandler location =
    LocationChange location


pagePath : Navigation.Location -> String
pagePath location =
    String.dropLeft 2 location.hash


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        ( alertList, alertListCmds ) =
            AlertList.init

        model =
            { mdl = Material.model
            , location = location
            , snackbar = Snackbar.model
            , alertList = alertList
            , page = Nothing
            , config = Nothing
            , sectionId = Nothing
            }

        loadConfig location =
            Task.attempt
                (\result ->
                    case result of
                        Err msg ->
                            ConfigFetchFail msg

                        Ok msg ->
                            ConfigFetchSucceed location msg
                )
                (Http.toTask <| Http.getString "/config.json")
    in
        if pagePath location == "" then
            let
                ( alertList, alertListCmds ) =
                    AlertList.add model.alertList AlertLevel.InfoLevel "Redirect to /home."
            in
                ( { model | alertList = alertList }
                , Cmd.batch
                    [ Layout.sub0 Mdl
                    , Cmd.map AlertListMsg alertListCmds
                    , loadConfig { location | hash = "#!/home" }
                    ]
                )
        else
            ( model
            , Cmd.batch
                [ Layout.sub0 Mdl
                , loadConfig location
                , Cmd.map AlertListMsg alertListCmds
                ]
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl mdlMsg ->
            Material.update mdlMsg model

        LocationChange location ->
            if pagePath location == "" then
                let
                    ( alertList, alertListCmds ) =
                        AlertList.add model.alertList AlertLevel.InfoLevel "Redirect to /home"
                in
                    ( { model | location = location, alertList = alertList }
                    , Cmd.batch
                        [ Cmd.map AlertListMsg alertListCmds
                        , Navigation.modifyUrl "/#!/home"
                        ]
                    )
            else
                let
                    ( page, pageFx, outMsg ) =
                        Page.init location

                    sectionRoute =
                        "/"
                            ++ (Maybe.withDefault "" <|
                                    List.head <|
                                        Maybe.withDefault [] <|
                                            List.tail (String.split "/" location.hash)
                               )

                    section =
                        Maybe.withDefault Nothing <|
                            flip Maybe.map
                                model.config
                                (\config ->
                                    flip List.Extra.find
                                        config.sections
                                        (\item -> item.route == sectionRoute)
                                )

                    blogTitle =
                        Maybe.withDefault "" <|
                            Maybe.map .title model.config

                    sectionTitle =
                        Maybe.withDefault "" <|
                            flip Maybe.map
                                section
                                (\s ->
                                    " - " ++ s.title
                                )
                in
                    ( { model
                        | location = location
                        , page = Just page
                        , sectionId = Maybe.map .id section
                      }
                    , Cmd.batch
                        [ title <| blogTitle ++ sectionTitle
                        , Cmd.map PageMsg pageFx
                        , if model.mdl.layout.isDrawerOpen then
                            Task.perform identity (Task.succeed HideDrawer)
                          else
                            Cmd.none
                        ]
                    )

        ConfigFetchFail httpError ->
            let
                message =
                    "The config is not loaded "
                        ++ case httpError of
                            Http.BadUrl info ->
                                "[BadUrl]: " ++ info

                            Http.Timeout ->
                                "[Timeout]."

                            Http.NetworkError ->
                                "[NetworkError]."

                            Http.BadStatus response ->
                                "[BadStatus]: "
                                    ++ (toString response.status.code)
                                    ++ " - "
                                    ++ response.status.message

                            Http.BadPayload info response ->
                                "[BadPayload]: " ++ info

                ( alertList, alertListCmds ) =
                    AlertList.add model.alertList AlertLevel.DangerLevel message
            in
                ( { model | alertList = alertList }
                , Cmd.batch
                    [ Cmd.map AlertListMsg alertListCmds
                    ]
                )

        ConfigFetchSucceed location config ->
            let
                blogTitle =
                    decodeString (field "title" string) config

                blogMode =
                    decodeString
                        (field "mode" <|
                            Json.Decode.map
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
                        (map5 SectionItem
                            (field "id" string)
                            (field "title" string)
                            (field "route" string)
                            (maybe (field "icon" string))
                            (field "placement" placementItemListDecoder)
                        )

                blogSections =
                    decodeString (field "sections" sectionItemListDecoder) config
            in
                case Result.map3 (,,) blogTitle blogMode blogSections of
                    Ok ( blogTitle, blogMode, blogSections ) ->
                        let
                            ( alertList, alertListCmds ) =
                                AlertList.add
                                    model.alertList
                                    AlertLevel.SuccessLevel
                                    "The config is loaded."
                        in
                            ( { model
                                | alertList = alertList
                                , config =
                                    Just
                                        { title = blogTitle
                                        , mode = blogMode
                                        , sections = blogSections
                                        }
                              }
                            , Cmd.batch
                                [ title blogTitle
                                , Navigation.modifyUrl <| "/" ++ location.hash
                                , Cmd.map AlertListMsg alertListCmds
                                ]
                            )

                    Err info ->
                        let
                            message =
                                "The config is not loaded. " ++ info

                            ( alertList, alertListCmds ) =
                                AlertList.add model.alertList AlertLevel.DangerLevel message
                        in
                            ( { model | alertList = alertList }
                            , Cmd.map AlertListMsg alertListCmds
                            )

        AlertListMsg alertListMsg ->
            let
                ( updatedAlertList, alertListCmds ) =
                    AlertList.update alertListMsg model.alertList
            in
                ( { model | alertList = updatedAlertList }, Cmd.map AlertListMsg alertListCmds )

        {--TODO: replace on OutMsg --}
        PageMsg pageMsg ->
            case pageMsg of
                Page.PageInfoFetchFail httpError ->
                    let
                        message =
                            case httpError of
                                Http.BadUrl info ->
                                    "[BadUrl]: " ++ info

                                Http.Timeout ->
                                    "[Timeout]"

                                Http.NetworkError ->
                                    "[NetworkError]"

                                Http.BadStatus response ->
                                    "[BadStatus]: "
                                        ++ (toString response.status.code)
                                        ++ " - "
                                        ++ response.status.message

                                Http.BadPayload info response ->
                                    "[BadPayload]: " ++ info

                        ( alertList, alertListCmds ) =
                            AlertList.add model.alertList AlertLevel.DangerLevel message
                    in
                        case model.page of
                            Just page ->
                                let
                                    ( updatedPage, pageCmds, outMsg ) =
                                        Page.update pageMsg page
                                in
                                    ( { model | page = Just updatedPage, alertList = alertList }
                                    , Cmd.batch
                                        [ Cmd.map AlertListMsg alertListCmds
                                        , Cmd.map PageMsg pageCmds
                                        ]
                                    )

                            Nothing ->
                                let
                                    ( alertList2, alertListCmds2 ) =
                                        AlertList.add alertList
                                            AlertLevel.DangerLevel
                                            "WTF: page is Nothing!"
                                in
                                    ( { model | alertList = alertList2 }
                                    , Cmd.batch
                                        [ Cmd.map AlertListMsg alertListCmds
                                        , Cmd.map AlertListMsg alertListCmds2
                                        ]
                                    )

                _ ->
                    case model.page of
                        Just page ->
                            let
                                ( updatedPage, pageCmds, outMsg ) =
                                    Page.update pageMsg page
                            in
                                case outMsg of
                                    Page.NoneOutMsg ->
                                        ( { model | page = Just updatedPage }
                                        , Cmd.map PageMsg pageCmds
                                        )

                                    Page.AlertOutMsg level info ->
                                        let
                                            ( alertList, alertListCmds ) =
                                                AlertList.add model.alertList level info
                                        in
                                            ( { model | page = Just updatedPage }
                                            , Cmd.batch
                                                [ Cmd.map PageMsg pageCmds
                                                , Cmd.map AlertListMsg alertListCmds
                                                ]
                                            )

                        Nothing ->
                            let
                                ( alertList, alertListCmds ) =
                                    AlertList.add model.alertList
                                        AlertLevel.DangerLevel
                                        "WTF: page is Nothing!"
                            in
                                ( { model | alertList = alertList }
                                , Cmd.map AlertListMsg alertListCmds
                                )

        SnackbarMsg snackbarMsg ->
            let
                ( snackbar, snackbarCmds ) =
                    Snackbar.update snackbarMsg model.snackbar
            in
                ( { model | snackbar = snackbar }
                , Cmd.map SnackbarMsg snackbarCmds
                )

        HideDrawer ->
            ( model, Task.perform identity (Task.succeed (Layout.toggleDrawer Mdl)) )


subscriptions : { model | mdl : Material.Model } -> Sub Msg
subscriptions =
    -- .mdl >> Layout.subs Mdl
    Material.subscriptions Mdl


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

        headerTitle =
            Layout.title [] <|
                Maybe.withDefault [ Html.text "" ] <|
                    flip Maybe.map
                        model.sectionId
                        (\sectionId ->
                            Maybe.withDefault [ Html.text "" ] <|
                                flip Maybe.map
                                    model.config
                                    (\config ->
                                        [ text config.title
                                        , span [ innerHtml "&nbsp;::&nbsp;" ] []
                                        , Html.text <|
                                            Maybe.withDefault "" <|
                                                flip Maybe.map
                                                    (List.Extra.find
                                                        (\item -> item.id == sectionId)
                                                        config.sections
                                                    )
                                                    (\section ->
                                                        section.title
                                                    )
                                        ]
                                    )
                        )
    in
        [ div [ class "mdl-layout--large-screen-only" ]
            [ Layout.row []
                [ headerTitle
                , Layout.spacer
                , Layout.navigation [] <|
                    List.map makeLink <|
                        Maybe.withDefault [] <|
                            flip Maybe.map
                                model.config
                                (\config ->
                                    List.filter
                                        (\item -> List.member HeaderPlacement item.placement)
                                        config.sections
                                )
                ]
            ]
        , div [ class "mdl-layout--small-screen-only" ]
            [ Layout.row []
                [ headerTitle
                ]
            ]
        ]


drawerView : Model -> List (Html Msg)
drawerView model =
    let
        makeLink item =
            Layout.link [ Layout.href ("/#!" ++ item.route) ]
                [ Maybe.withDefault (span [] [ text "" ]) <|
                    Maybe.map Icon.i item.icon
                , text item.title
                ]
    in
        [ Layout.title []
            [ Maybe.withDefault (text "") <|
                flip Maybe.map
                    model.config
                    (\config ->
                        Maybe.withDefault (text "") <|
                            flip Maybe.map
                                model.sectionId
                                (\sectionId ->
                                    span []
                                        [ text config.title
                                        , span [ innerHtml "&nbsp;::&nbsp;" ] []
                                        , text <|
                                            Maybe.withDefault "" <|
                                                Maybe.map .title <|
                                                    List.Extra.find
                                                        (\item -> item.id == sectionId)
                                                        config.sections
                                        ]
                                )
                    )
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
        , Layout.navigation [] <|
            Maybe.withDefault [] <|
                flip Maybe.map
                    model.config
                    (\config ->
                        List.map makeLink <|
                            List.filter
                                (\item -> List.member DrawerPlacement item.placement)
                                config.sections
                    )
        ]


mainView : Model -> List (Html Msg)
mainView model =
    [ Grid.grid []
        [ Grid.cell [ Grid.size All 8, Grid.offset Desktop 2, Elevation.e3 ]
            [ Maybe.withDefault (text "Page do'nt loaded.") <|
                flip Maybe.map
                    model.page
                    (\page -> Html.map PageMsg (Page.view page))
            ]
        , Grid.cell [ Grid.size All 2 ] <|
            Maybe.withDefault [] <|
                flip Maybe.map
                    model.config
                    (\config ->
                        case config.mode of
                            DevelopmentMode ->
                                [ Html.map AlertListMsg (AlertList.view model.alertList) ]

                            _ ->
                                []
                    )
        ]
    , Layout.spacer
    , Footer.mini []
        { left =
            Footer.left []
                [ Footer.logo []
                    [ Footer.html <| span [ innerHtml "&copy; 2016" ] []
                    ]
                , Footer.links [] <|
                    Maybe.withDefault [] <|
                        flip Maybe.map
                            model.config
                            (\config ->
                                flip List.map
                                    config.sections
                                    (\item ->
                                        Maybe.withDefault (Footer.html <| text "") <|
                                            flip Maybe.map
                                                (List.Extra.find
                                                    (\item -> item == FooterPlacement)
                                                    item.placement
                                                )
                                                (\_ ->
                                                    Footer.linkItem
                                                        [ Footer.href <| "/#!" ++ item.route ]
                                                        [ Footer.html <| text item.title ]
                                                )
                                    )
                            )
                ]
        , right =
            Footer.right []
                [ Footer.html <|
                    a
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
    , Snackbar.view model.snackbar |> Html.map SnackbarMsg
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
