port module Main exposing (main)

import Html exposing (..)
import Html.App
import Html.Attributes exposing (class, classList, href, property, style)

import Http
import Json.Decode exposing (..)
import Json.Encode
import List exposing (head, tail)
import String
import Task

import Material
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
    title : String,
    navigation : List NavigationItem,
    toasts : List String,
    snackbar : Snackbar.Model (),
    root : String,
    isConfigLoaded : Maybe Bool
  }

type alias NavigationItem =
  {
    title : String,
    route : String,
    icon : Maybe String
  }


type Msg =
  Mdl Material.Msg |
  ConfigFetchSucceed { path : String, query : String } String |
  ConfigFetchFail Http.Error |
  SnackbarMsg (Snackbar.Msg ()) |
  PageMsg Page.Msg


port title : String -> Cmd msg


init : Result String  { path : String, query : String} -> (Model, Cmd Msg)
init result =
  let
    model =
      {
        mdl = Material.model,
        page = Nothing,
        title = "",
        navigation = [],
        toasts = [],
        snackbar = Snackbar.model,
        root = "",
        isConfigLoaded = Nothing
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
      navigationItemListDecoder = list
        (
          object3 NavigationItem
            ("title" := string) ("route" := string) (maybe ("icon" := string))
        )
      blogNavigation = decodeString ("navigation" := navigationItemListDecoder) config
      blogRoot = decodeString ( "root" := string ) config
    in
      case Result.map3 (,,) blogTitle blogNavigation blogRoot of
        Ok (title', navigation', root') ->
          let
            (snackbar', snackbarCmds) =
              Snackbar.add (Snackbar.toast () "The config is loaded.") model.snackbar
          in
            (
              {
                model |
                  title = title',
                  navigation = navigation',
                  root = root',
                  snackbar = snackbar',
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
              { model | snackbar = snackbar', isConfigLoaded = Just False },
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
                    title <| model.title ++ " - " ++ updatedPage.title,
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
                    title <| model.title ++ " - " ++ updatedPage.title,
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
                  title <| model.title ++ " - " ++ updatedPage.title,
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
          (page, pageFx) = Page.init parsedUrl.path parsedUrl.query model.root
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
                [
                  text model.title,
                  span [ property "innerHTML" <| Json.Encode.string "&nbsp;::&nbsp;"] [],
                  text
                    (
                      case model.page of
                        Just page ->
                          page.title
                        Nothing ->
                          "Failed to load page."
                    )
                ],
              Layout.spacer,
              Layout.navigation [] (List.map makeLink model.navigation)
            ]
        ],
      div [class "mdl-layout--small-screen-only"]
        [
          Layout.row []
            [
              Layout.title []
                [
                  text model.title,
                  span [ property "innerHTML" <| Json.Encode.string "&nbsp;::&nbsp;"] [],
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
          text model.title,
          span [ property "innerHTML" <| Json.Encode.string "&nbsp;::&nbsp;"] [],
          text
            (
              case model.page of
                Just page ->
                  page.title
                Nothing ->
                  "Failed to load page."
            )
        ],
        hr [] [],
        Layout.navigation [] (List.map makeLink model.navigation)
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
                    text "The config is not loaded. Restart the application later or contact the developer."
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
