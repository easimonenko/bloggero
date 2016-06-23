port module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (class, href)

import Http
import Json.Decode exposing (..)
import List exposing (head, tail)
import String
import Task

import Material
import Material.Icon as Icon
import Material.Layout as Layout
import Material.Options as Options
import Material.Snackbar as Snackbar

import Navigation
import UrlParser exposing ((</>))


type alias Page =
  {
    path : String,
    query : String
  }

type alias NavigationItem =
  {
    title : String,
    route : String,
    icon : Maybe String
  }

type alias Model =
  {
    page : Page,
    configLoaded : Bool,
    title : String,
    navigation : List NavigationItem,
    toast : String
  }

type Msg =
  Mdl Material.Msg |
  ConfigFetchSucceed String |
  ConfigFetchFail Http.Error |
  ShowToast String |
  ShowErrorToast String

port title : String -> Cmd msg

init result =
  urlUpdate result { page = { path = "", query = "" }, configLoaded = False, title = "", navigation = [], toast = "" }

headerView model =
  let
    makeA item =
      a
        [ class "mdl-navigation__link", href ("/#!" ++ item.route) ]
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
      Layout.row []
        [
          Layout.title [] [ text model.title ],
          Layout.spacer,
          Layout.navigation [] (List.map makeA model.navigation)
        ]
    ]


drawerView = [ div [] [] ]

mainView model =
  [ main' []
    [
      div []
        [ text <| toString <| "path: " ++ model.page.path ++ " query: " ++ model.page.query ],
      div []
        [ text <| toString model.toast ]
    ]
  ]

tabsView = ( [], [] )

view model = Layout.render Mdl Material.model
  [
    Layout.fixedHeader
  ]
  {
    header = headerView model,
    drawer = drawerView,
    main = mainView model,
    tabs = tabsView
  }

update action model = case action of
  {--ConfigFetchSucceed config -> Snackbar.add (Snackbar.toast () config) model
  ConfigFetchFail Http.Timeout -> Snackbar.add (Snackbar.toast () "Timeout") model
  ConfigFetchFail Http.NetworkError -> Snackbar.add (Snackbar.toast () "NetworkError") model
  ConfigFetchFail (Http.UnexpectedPayload str) -> Snackbar.add (Snackbar.toast () str) model
  ConfigFetchFail (Http.BadResponse code str) -> Snackbar.add (Snackbar.toast () ((toString code) ++ str)) model-}
  ConfigFetchSucceed config ->
    let
      blogTitle = case decodeString ("title" := string) config of
        Ok str -> str
        Err _ -> ""
      navigationItemListDecoder = list
        (
          object3 NavigationItem
            ("title" := string) ("route" := string) (maybe ("icon" := string))
        )
      blogNavigation = case decodeString ("navigation" := navigationItemListDecoder) config of
        Ok list -> list
        Err _ -> []
    in
      (
        { model | configLoaded = True, title = blogTitle, navigation = blogNavigation }, title blogTitle
      )
  _ -> ( model, Cmd.none )

subscriptions model = Sub.none

hashParser location = UrlParser.parse identity pageParser location.hash

queryString =
  UrlParser.custom "Getting query string failed"
    (
      \ str ->
        case String.split "?" str of
          (_ :: q :: _) -> Ok q
          _ -> Ok ""
    )

pageParser =
  UrlParser.oneOf
    [
      UrlParser.format (\ path query -> { path = path, query = query }) ( UrlParser.s "#!" </> (UrlParser.string </> queryString)),
      UrlParser.format (\ path -> { path = path, query = "" }) ( UrlParser.s "#!" </> UrlParser.string ),
      UrlParser.format { path = "", query = "" } ( UrlParser.s "#!" )
    ]

urlUpdate result model =
  if model.configLoaded
    then
      case result of
        Err info -> ( { model | toast = info }, Navigation.modifyUrl "/#!/" )
        Ok page -> ( { model | page = page, toast = "" }, Cmd.none )
    else
      ( model,  Task.perform ConfigFetchFail ConfigFetchSucceed (Http.getString "/config.json") )

main = Navigation.program
  (Navigation.makeParser hashParser)
  {
    init = init,
    view = view,
    subscriptions = subscriptions,
    update = update,
    urlUpdate = urlUpdate
  }
