port module Main exposing (main)

import Html exposing (..)
import Html.App
import Html.Attributes exposing (class, classList, href, property, style)

import Html.Attributes.Extra exposing (innerHtml)

import Http
import Json.Decode exposing ((:=), decodeString, list, maybe, object4, string)
import Json.Encode
import List exposing (filter, head, map, member, tail)
import String
import Task

import Material
import Material.Button as Button
import Material.Footer as Footer
import Material.Icon as Icon
import Material.Layout as Layout
import Material.Snackbar as Snackbar

import Navigation

import Page


main : Program Never
main = Navigation.program
  (Navigation.makeParser urlParser)
  {
    init = init,
    view = view,
    subscriptions = subscriptions,
    update = update,
    urlUpdate = urlUpdate
  }


type alias Model =
  {
    mdl : Material.Model,
    page : Maybe Page.Model,
    snackbar : Snackbar.Model (),
    config :
      {
        title : String,
        root : String,
        mode : Mode,
        sections : List SectionItem
      },
    isConfigLoaded : Maybe Bool,
    debugMessages : List String
  }

type Mode =
  DevelopmentMode |
  ProductionMode |
  UnknownMode

type alias SectionItem =
  {
    title : String,
    route : String,
    icon : Maybe String,
    placement : List Placement
  }

type Placement =
  HeaderPlacement |
  DrawerPlacement |
  FooterPlacement |
  SiteMapPlacement |
  UnknownPlacement

type Msg =
  Mdl Material.Msg |
  ConfigFetchSucceed { path : String, query : String } String |
  ConfigFetchFail Http.Error |
  SnackbarMsg (Snackbar.Msg ()) |
  PageMsg Page.Msg |
  HideDrawer


port title : String -> Cmd msg


init : Result String  { path : String, query : String} -> (Model, Cmd Msg)
init result =
  let
    model =
      {
        mdl = Material.model,
        snackbar = Snackbar.model,
        page = Nothing,
        config =
          {
            title = "",
            root = "",
            mode = DevelopmentMode,
            sections = []
          },
        isConfigLoaded = Nothing,
        debugMessages = []
      }
    loadConfig parsedUrl =
      Task.perform ConfigFetchFail (ConfigFetchSucceed parsedUrl) (Http.getString "/config.json")
  in
    case result of
      Err info ->
        let
          (snackbar', snackbarCmds) =
            Snackbar.add (Snackbar.toast () <| "Parsing URL: " ++ info) model.snackbar
        in
          (
            { model | snackbar = snackbar' },
            Cmd.batch
              [
                Layout.sub0 Mdl,
                Cmd.map SnackbarMsg snackbarCmds,
                loadConfig { path = "/error/unknown-url", query = "" }
              ]
          )
      Ok pageUrl ->
        if
          pageUrl.path == ""
        then
          let
            (snackbar', snackbarCmds) =
              Snackbar.add (Snackbar.toast () <| "Redirect to /home.") model.snackbar
          in
            (
              { model | snackbar = snackbar' },
              Cmd.batch
                [
                  Layout.sub0 Mdl,
                  Cmd.map SnackbarMsg snackbarCmds,
                  loadConfig { path = "/home", query = pageUrl.query }
                ]
            )
        else
          (
            model,
            Cmd.batch [ Layout.sub0 Mdl, loadConfig pageUrl ]
          )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  Mdl mdlMsg ->
    Material.update Mdl mdlMsg model
  ConfigFetchFail Http.Timeout ->
    let
      (snackbar', snackbarCmds) =
        Snackbar.add (Snackbar.toast () "The config is not loaded [Timeout].") model.snackbar
    in
      (
        { model | snackbar = snackbar', isConfigLoaded = Just False },
        Cmd.map SnackbarMsg snackbarCmds
      )
  ConfigFetchFail Http.NetworkError ->
    let
      (snackbar', snackbarCmds) =
        Snackbar.add (Snackbar.toast () "The config is not loaded [NetworkError].") model.snackbar
    in
      (
        { model | snackbar = snackbar', isConfigLoaded = Just False },
        Cmd.map SnackbarMsg snackbarCmds
      )
  ConfigFetchFail (Http.UnexpectedPayload info) ->
    let
      (snackbar', snackbarCmds) =
        Snackbar.add
          (Snackbar.toast () <| "The config is not loaded [UnexpectedPayload]: " ++ info)
          model.snackbar
    in
      (
        { model | snackbar = snackbar', isConfigLoaded = Just False },
        Cmd.map SnackbarMsg snackbarCmds
      )
  ConfigFetchFail (Http.BadResponse code info) ->
    let
      (snackbar', snackbarCmds) =
        Snackbar.add
          (Snackbar.toast () <| "The config is not loaded [BadResponse]: " ++ (toString code) ++ " - " ++ info)
          model.snackbar
    in
      (
        { model | snackbar = snackbar', isConfigLoaded = Just False },
        Cmd.map SnackbarMsg snackbarCmds
      )
  ConfigFetchSucceed pageUrl config ->
    let
      blogTitle = decodeString ("title" := string) config
      blogRoot = decodeString ("root" := string) config
      blogMode = decodeString
        (
          "mode" := Json.Decode.map
            (\
              item -> case item of
                "development" -> DevelopmentMode
                "production" -> ProductionMode
                _ -> UnknownMode
            )
            string
        )
        config
      placementItemListDecoder = list
        (
          Json.Decode.map
            (\
              item -> case item of
                "header" -> HeaderPlacement
                "drawer" -> DrawerPlacement
                "footer" -> FooterPlacement
                "sitemap" -> SiteMapPlacement
                _ -> UnknownPlacement
            )
            string
        )
      sectionItemListDecoder = list
        (
          object4 SectionItem
            ("title" := string) ("route" := string) (maybe ("icon" := string)) ("placement" := placementItemListDecoder)
        )
      blogSections = decodeString ("sections" := sectionItemListDecoder) config
    in
      case Result.map4 (,,,) blogTitle blogRoot blogMode blogSections of
        Ok (title', root', mode', sections') ->
          let
            (snackbar', snackbarCmds) =
              Snackbar.add (Snackbar.toast () "The config is loaded.") model.snackbar
          in
            (
              {
                model |
                  snackbar = snackbar',
                  config =
                    let
                      modelConfig = model.config
                    in
                      {
                        modelConfig |
                          title = title',
                          root = root',
                          mode = mode',
                          sections = sections'
                      },
                  isConfigLoaded = Just True
              },
              Cmd.batch
                [
                  title title',
                  Navigation.modifyUrl <| "/#!" ++ pageUrl.path,
                  Cmd.map SnackbarMsg snackbarCmds
                ]
            )
        Err info ->
          let
            (snackbar', snackbarCmds) =
              Snackbar.add (Snackbar.toast () <| "The config is not loaded. " ++ info) model.snackbar
          in
            (
              {
                model |
                  snackbar = snackbar',
                  config =
                    let
                      modelConfig = model.config
                    in
                      {
                        modelConfig |
                          mode = DevelopmentMode
                      },
                  isConfigLoaded = Just False,
                  debugMessages =
                    let
                      modelDebugMessages = model.debugMessages
                    in
                      info :: modelDebugMessages
              },
              Cmd.map SnackbarMsg snackbarCmds
            )
  PageMsg pageMsg ->
    case pageMsg of
      Page.PageInfoFetchFail pageUrl (Http.BadResponse statusCode statusInfo) ->
        let
          (snackbar', snackbarCmds) =
            Snackbar.add
              (Snackbar.toast () <| "Bad response: " ++ (toString statusCode) ++ " - " ++ statusInfo)
              model.snackbar
        in
          (
            { model | snackbar = snackbar' },
            Cmd.batch
              [
                Cmd.map SnackbarMsg snackbarCmds,
                Navigation.modifyUrl <| "/#!/error/bad-response/" ++ toString statusCode
              ]
          )
      Page.PageInfoFetchFail pageUrl Http.Timeout ->
        let
          (snackbar', snackbarCmds) =
            Snackbar.add
              (Snackbar.toast () "Http Timeout.")
              model.snackbar
        in
          case model.page of
            Just page ->
              let
                (updatedPage, pageCmds) = Page.update pageMsg page
              in
                (
                  { model | page = Just updatedPage, snackbar = snackbar' },
                  Cmd.batch
                  [
                    title <| model.config.title ++ " - " ++ updatedPage.title,
                    Cmd.map SnackbarMsg snackbarCmds,
                    Cmd.map PageMsg pageCmds
                  ]
                )
            Nothing ->
              let
                (snackbar', snackbarCmds) = Snackbar.add
                  (Snackbar.toast () "WTF: page is Nothing!")
                  model.snackbar
              in
                (
                  { model | snackbar = snackbar' },
                  Cmd.map SnackbarMsg snackbarCmds
                )
      Page.PageInfoFetchFail pageUrl Http.NetworkError ->
        let
          (snackbar', snackbarCmds) =
            Snackbar.add
              (Snackbar.toast () "Network error.")
              model.snackbar

        in
          case model.page of
            Just page ->
              let
                (updatedPage, pageCmds) = Page.update pageMsg page
              in
                (
                  { model | page = Just updatedPage, snackbar = snackbar' },
                  Cmd.batch
                  [
                    title <| model.config.title ++ " - " ++ updatedPage.title,
                    Cmd.map SnackbarMsg snackbarCmds,
                    Cmd.map PageMsg pageCmds
                  ]
                )
            Nothing ->
              let
                (snackbar', snackbarCmds) = Snackbar.add
                  (Snackbar.toast () "WTF: page is Nothing!")
                  model.snackbar
              in
                (
                  { model | snackbar = snackbar' },
                  Cmd.map SnackbarMsg snackbarCmds
                )
      Page.PageInfoFetchFail pageUrl (Http.UnexpectedPayload info) ->
        let
          (snackbar', snackbarCmds) =
            Snackbar.add
              (Snackbar.toast () <| "Unexpected payload: " ++ info)
              model.snackbar
        in
          (
            { model | snackbar = snackbar' },
            Cmd.batch
              [
                Cmd.map SnackbarMsg snackbarCmds,
                Navigation.modifyUrl <| "/#!/error/unexpected-payload"
              ]
          )
      Page.PageInfoFetchSucceed _ ->
        case model.page of
          Just page ->
            let
              (updatedPage, pageCmds) = Page.update pageMsg page
            in
              (
                { model | page = Just updatedPage},
                Cmd.batch
                [
                  title <| model.config.title ++ " - " ++ updatedPage.title,
                  Cmd.map PageMsg pageCmds
                ]
              )
          Nothing ->
            let
              (snackbar', snackbarCmds) = Snackbar.add
                (Snackbar.toast () "WTF: page is Nothing!")
                model.snackbar
            in
              (
                { model | snackbar = snackbar' },
                Cmd.map SnackbarMsg snackbarCmds
              )
      _ ->
        case model.page of
          Just page ->
            let
              (updatedPage, pageCmds) = Page.update pageMsg page
            in
              (
                { model | page = Just updatedPage },
                Cmd.map PageMsg pageCmds
              )
          Nothing ->
            let
              (snackbar', snackbarCmds) = Snackbar.add
                (Snackbar.toast () "WTF: page is Nothing!")
                model.snackbar
            in
              (
                { model | snackbar = snackbar' },
                Cmd.map SnackbarMsg snackbarCmds
              )
  SnackbarMsg snackbarMsg ->
    let
      (snackbar', snackbarCmds) = Snackbar.update snackbarMsg model.snackbar
    in
      (
        { model | snackbar = snackbar' },
        Cmd.map SnackbarMsg snackbarCmds
      )
  HideDrawer ->
    ( model, Task.perform identity identity (Task.succeed (Layout.toggleDrawer Mdl))  )


subscriptions = .mdl >> Layout.subs Mdl


urlParser
  : Navigation.Location
  -> Result String { path : String, query : String }
urlParser location =
  Ok { path = String.dropLeft 2 location.hash, query = String.dropLeft 1 location.search }


urlUpdate : Result String { path : String, query : String } -> Model -> (Model, Cmd Msg)
urlUpdate result model =
  case result of
    Err info ->
      let
        (snackbar', snackbarCmds) = Snackbar.add
          (Snackbar.toast () <| "Unknown URL: " ++ info)
          model.snackbar
      in
        (
          { model | snackbar = snackbar' },
          Cmd.batch
          [
            Cmd.map SnackbarMsg snackbarCmds,
            Navigation.modifyUrl "/#!/error/unknown-url"
          ]
        )
    Ok parsedUrl ->
      if
        parsedUrl.path == ""
      then
        let
          (snackbar', snackbarCmds) = Snackbar.add
            (Snackbar.toast () <| "Redirect to /home")
            model.snackbar
        in
          (
            { model | snackbar = snackbar' },
            Cmd.batch
            [
              Cmd.map SnackbarMsg snackbarCmds,
              Navigation.modifyUrl "/#!/home"
            ]
          )
      else
        let
          (page, pageFx) = Page.init parsedUrl.path parsedUrl.query model.config.root
        in
          (
            { model | page = Just page },
            Cmd.map PageMsg pageFx
          )


headerView : Model -> List (Html Msg)
headerView model =
  let
    makeLink item =
      Layout.link [ Layout.href ("/#!" ++ item.route) ]
        [
          (
            case item.icon of
              Nothing -> span [] [text ""]
              Just iconName -> Icon.i iconName
          ),
          text item.title
        ]
  in
    [
      div [class "mdl-layout--large-screen-only"]
        [
          Layout.row []
            [
              Layout.title []
                (
                  case model.isConfigLoaded of
                    Just True ->
                      [
                        text model.config.title,
                        span [ innerHtml "&nbsp;::&nbsp;"] [],
                        text
                          (
                            case model.page of
                              Just page ->
                                page.title
                              Nothing ->
                                "Failed to load page."
                          )
                      ]
                    _ ->
                      [
                        text ""
                      ]
                ),
              Layout.spacer,
              Layout.navigation []
                (
                  map makeLink <|
                    filter (\item -> member HeaderPlacement item.placement) model.config.sections
                )
            ]
        ],
      div [class "mdl-layout--small-screen-only"]
        [
          Layout.row []
            [
              Layout.title []
                [
                  text model.config.title,
                  span [ innerHtml "&nbsp;::&nbsp;"] [],
                  text
                    (
                      case model.page of
                        Just page ->
                          page.title
                        Nothing ->
                          "Failed to load page."
                    )
                ]
            ]
        ]
    ]


drawerView : Model -> List (Html Msg)
drawerView model =
  let
    makeLink item =
      Layout.link [ Layout.href ("/#!" ++ item.route) ]
        [
          (
            case item.icon of
              Nothing -> span [] [text ""]
              Just iconName -> Icon.i iconName
          ),
          text item.title
        ]
  in
    [
      Layout.title []
        [
          text model.config.title,
          span [ innerHtml "&nbsp;::&nbsp;"] [],
          text
            (
              case model.page of
                Just page ->
                  page.title
                Nothing ->
                  "Failed to load page."
            ),
          Button.render Mdl [0] model.mdl
            [
              Button.icon,
              Button.colored,
              Button.ripple,
              Button.onClick HideDrawer
            ]
            [
              Icon.i "keyboard_arrow_left"
            ]
        ],
        hr [] [],
        Layout.navigation []
          (
            map makeLink <|
              filter (\item -> member DrawerPlacement item.placement) model.config.sections
          )
    ]


mainView : Model -> List (Html Msg)
mainView model =
  [
    div []
      [
        case model.isConfigLoaded of
          Just True ->
            case model.page of
              Just page -> Html.App.map PageMsg (Page.view page)
              Nothing -> text "Failed to load page."
          Just False ->
            div
              [
                style
                  [
                    ("width", "100%"),
                    ("height", "100%"),
                    ("position", "fixed"),
                    ("top", "0"),
                    ("left", "0"),
                    ("display", "flex"),
                    ("align-items", "center"),
                    ("justify-content", "center"),
                    ("overflow", "auto")
                  ]
              ]
              [
                div
                  [
                    style
                      [
                        ("background", "lightyellow")
                      ]
                  ]
                  [
                    text "The config is not loaded. Restart the application later or contact the developer.",
                    case model.config.mode of
                      DevelopmentMode ->
                        text <| " --> " ++ String.join " --> " model.debugMessages
                      ProductionMode ->
                        text ""
                      UnknownMode ->
                        text " --> Unknown mode."
                  ]
              ]
          Nothing ->
            div
              [
                style
                  [
                    ("width", "100%"),
                    ("height", "100%"),
                    ("position", "fixed"),
                    ("top", "0"),
                    ("left", "0"),
                    ("display", "flex"),
                    ("align-items", "center"),
                    ("justify-content", "center"),
                    ("overflow", "auto")
                  ]
              ]
              [
                div
                  [
                    style
                      [
                        ("background", "lightgreen")
                      ]
                  ]
                  [
                    text "The application is downloaded. Please wait a bit."
                  ]
              ]
      ],
    Layout.spacer,
    Footer.mini []
      {
        left = Footer.left []
          [
            Footer.logo []
              [
                Footer.html <| span [ property "innerHTML" <| Json.Encode.string "&copy; 2016"] []
              ]
          ],
        right = Footer.right []
          [
            Footer.html <|
              a
                [
                  href "https://github.com/easimonenko/bloggero-elm-mdl",
                  Html.Attributes.property "role" (Json.Encode.string "button"),
                  Html.Attributes.title "GitHub"
                ]
                [
                  i
                    [
                      class "fa fa-github fa-3x fa-hover",
                      property "aria-hidden" (Json.Encode.bool True)
                    ]
                    []
                ]
          ]
      },
    Snackbar.view model.snackbar |> Html.App.map SnackbarMsg
  ]


tabsView = ( [], [] )


view : Model -> Html Msg
view model = Layout.render Mdl model.mdl
  [
    Layout.fixedHeader,
    Layout.fixedTabs,
    Layout.seamed,
    Layout.waterfall True
  ]
  {
    header = headerView model,
    drawer = drawerView model,
    main = mainView model,
    tabs = tabsView
  }
