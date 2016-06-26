port module Main exposing (main)

import Html exposing (..)
import Html.App
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

import Page


main : Program Never
main = Navigation.program
  (Navigation.makeParser hashParser)
  {
    init = init,
    view = view,
    subscriptions = subscriptions,
    update = update,
    urlUpdate = urlUpdate
  }


type alias Model =
  {
    page : Maybe Page.Model,
    title : String,
    navigation : List NavigationItem,
    toasts : List String,
    root : String
  }

type alias NavigationItem =
  {
    title : String,
    route : String,
    icon : Maybe String
  }


type Msg =
  Mdl Material.Msg |
  ConfigFetchSucceed String |
  ConfigFetchFail Http.Error |
  ShowToast String |
  ShowErrorToast String |
  PageMsg Page.Msg


port title : String -> Cmd msg


init : Result String  { path : String, query : String} -> (Model, Cmd Msg)
init result =
  let
    model =
      {
        page = Nothing,
        title = "",
        navigation = [],
        toasts = [],
        root = ""
      }
    loadConfig = Task.perform ConfigFetchFail ConfigFetchSucceed (Http.getString "/config.json")
  in
    case result of
      Err info ->
        (
          { model | toasts = info :: model.toasts },
          Cmd.batch [loadConfig, Navigation.modifyUrl "/#!/404"]
        )
      Ok page ->
        if
          page.path == ""
        then
          let
            modelPage = model.page
          in
          (
            --{ model | page = { modelPage | path = "home" }, toasts = "Redirect to /home" :: model.toasts },
            { model | toasts = "Redirect to /home" :: model.toasts },
            Cmd.batch [ loadConfig, Navigation.modifyUrl "/#!/home" ]
          )
        else
          (
            --{ model | page = page, toasts = (if page.path == "404" then model.toast else "") :: model.toasts },
            model,
            loadConfig
          )


update : Msg -> Model -> (Model, Cmd Msg)
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
      blogRoot = case decodeString ( "root" := string ) config of
        Ok str -> str
        Err _ -> ""
    in
      (
        { model | title = blogTitle, navigation = blogNavigation, root = blogRoot },
        title blogTitle
      )
  PageMsg pageMsg ->
    case model.page of
      Just page ->
        let
          (updatedPage, pageCmds) = Page.update pageMsg page
        in
          ( { model | page = Just updatedPage}, Cmd.map PageMsg pageCmds )
      Nothing ->
        ( { model | toasts = "WTF: page is Nothing!" :: model.toasts }, Cmd.none )
  _ ->
    ( model, Cmd.none )


subscriptions model = Sub.none


hashParser
  : { a | hash : String }
  -> Result String { path : String, query : String }
hashParser location = UrlParser.parse identity pageParser location.hash

pageParser =
  UrlParser.oneOf
    [
      UrlParser.format
        (
           \ url ->
              case String.split "?" url of
                [] -> { path = "", query = "" }
                [ path ] -> { path = path, query = "" }
                ( path :: query :: _ ) -> { path = path, query = query }
        )
        ( UrlParser.s "#!" </> UrlParser.string ),
      UrlParser.format
        { path = "", query = "" }
        ( UrlParser.s "" )
    ]


urlUpdate : Result String { path : String, query : String } -> Model -> (Model, Cmd Msg)
urlUpdate result model =
  case result of
    Err info ->
      (
        { model | toasts = info :: model.toasts },
        Navigation.modifyUrl "/#!/404"
      )
    Ok page ->
      if
        page.path == ""
      then
        (
           { model | toasts = "Redirect to /home" :: model.toasts },
           Navigation.modifyUrl "/#!/home"
        )
      else
        let
          (page, pageFx) = Page.init page.path page.query model.root
        in
          (
            { model | page = Just page },
            Cmd.map PageMsg pageFx
          )


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
          span [] [ text "|" ],
          Layout.spacer,
          case model.page of
            Just page ->
              span [] [ text page.title ]
            Nothing ->
              span [] [ text "Page do'nt loaded" ],
          Layout.spacer,
          span [] [ text "|" ],
          Layout.spacer,
          Layout.navigation [] (List.map makeA model.navigation)
        ]
    ]


drawerView = [ div [] [] ]


mainView model =
  [ main' []
    [
      div []
        [
          let
            (path, query) = case model.page of
              Just page -> (page.path, page.query)
              Nothing -> ("", "")
          in
            text <| "path: " ++ path ++ " query: " ++ query
        ],
      div []
        [ text <| String.join "<--" model.toasts ],
      case model.page of
        Just page -> Html.App.map PageMsg (Page.view page)
        Nothing -> text "Page do'nt loaded"
    ]
  ]


tabsView = ( [], [] )


view : Model -> Html Msg
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
