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
  ConfigFetchSucceed { path : String, query : String } String |
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
    loadConfig parsedUrl =
      Task.perform ConfigFetchFail (ConfigFetchSucceed parsedUrl) (Http.getString "/config.json")
  in
    case result of
      Err info ->
        (
          { model | toasts = info :: model.toasts },
          loadConfig { path = "/error/unknown-url", query = "" }
        )
      Ok pageUrl ->
        if
          pageUrl.path == ""
        then
          (
            { model | toasts = "Redirect to /home" :: model.toasts },
            loadConfig { path = "/home", query = pageUrl.query }
          )
        else
          (
            model,
            loadConfig pageUrl
          )


update : Msg -> Model -> (Model, Cmd Msg)
update action model = case action of
  {--ConfigFetchSucceed config -> Snackbar.add (Snackbar.toast () config) model
  ConfigFetchFail Http.Timeout -> Snackbar.add (Snackbar.toast () "Timeout") model
  ConfigFetchFail Http.NetworkError -> Snackbar.add (Snackbar.toast () "NetworkError") model
  ConfigFetchFail (Http.UnexpectedPayload str) -> Snackbar.add (Snackbar.toast () str) model
  ConfigFetchFail (Http.BadResponse code str) -> Snackbar.add (Snackbar.toast () ((toString code) ++ str)) model-}
  ConfigFetchSucceed pageUrl config ->
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
        Cmd.batch [ title blogTitle, Navigation.modifyUrl <| "/#!" ++ pageUrl.path ]
      )
  PageMsg pageMsg ->
    case pageMsg of
      Page.PageFetchFail (Http.BadResponse statusCode statusInfo) ->
        (
          { model | toasts = statusInfo :: model.toasts },
          Navigation.modifyUrl <| "/#!/error/bad-response/" ++ toString statusCode
        )
      Page.PageFetchFail Http.Timeout ->
        (
          { model | toasts = "Http Timeout" :: model.toasts },
          Navigation.modifyUrl <| "/#!/error/timeout"
        )
      Page.PageFetchFail Http.NetworkError ->
        (
          { model | toasts = "Network Error" :: model.toasts },
          Navigation.modifyUrl <| "/#!/error/network-error"
        )
      Page.PageFetchFail (Http.UnexpectedPayload _) ->
        (
          { model | toasts = "Unexpected Payload" :: model.toasts },
          Navigation.modifyUrl <| "/#!/error/unexpected-payload"
        )
      Page.PageFetchSucceed _ ->
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
            ( { model | toasts = "WTF: page is Nothing!" :: model.toasts }, Cmd.none )
  _ ->
    ( model, Cmd.none )


subscriptions model = Sub.none


urlParser
  : Navigation.Location
  -> Result String { path : String, query : String }
urlParser location =
  Ok { path = String.dropLeft 2 location.hash, query = String.dropLeft 1 location.search }


urlUpdate : Result String { path : String, query : String } -> Model -> (Model, Cmd Msg)
urlUpdate result model =
  case result of
    Err info ->
      (
        { model | toasts = info :: model.toasts },
        Navigation.modifyUrl "/#!/error/unknown-url"
      )
    Ok parsedUrl ->
      if
        parsedUrl.path == ""
      then
        (
          { model | toasts = "Redirect to /home" :: model.toasts },
          Navigation.modifyUrl "/#!/home"
        )
      else
        let
          (page, pageFx) = Page.init parsedUrl.path parsedUrl.query model.root
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
          Layout.title []
            [
              text (case model.page of
                Just page ->
                  page.title
                Nothing ->
                  "Page do'nt loaded")
            ],
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
      case model.page of
        Just page -> Html.App.map PageMsg (Page.view page)
        Nothing -> text "Page do'nt loaded",
      hr [] [],
      div []
        [ text <| String.join "<--" model.toasts ]
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
