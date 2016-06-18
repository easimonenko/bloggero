port module Main exposing (main)

import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (class, href)

import Http
import Json.Decode exposing (..)

import Material
import Material.Icon as Icon
import Material.Layout as Layout
import Material.Options as Options
import Material.Snackbar as Snackbar

import String
import Task

type alias Model =
  {
    title : String,
    config : String,
    toast : String
  }

type Msg =
  Mdl Material.Msg |
  ConfigFetchSucceed String |
  ConfigFetchFail Http.Error |
  ShowToast String |
  ShowErrorToast String

port title : String -> Cmd msg

init =
  (
    {
      title = "",
      config = "",
      toast = ""
    },
    Task.perform ConfigFetchFail ConfigFetchSucceed (Http.getString "/config.json")
  )

headerView model = [ Layout.row []
    [
      Layout.title [] [ text model.title ],
      Layout.spacer,
      Layout.navigation []
        [
          a [ class "mdl-navigation__link", href "/#!/"] [ Icon.i "home", text "Home" ]
        ]
    ]
  ]

drawerView = [ div [] [] ]

mainView model =
  [ main' []
    [
      text model.toast
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
    in
      (
        { model | title = blogTitle }, title blogTitle
      )
  _ -> ( model, Cmd.none )

subscriptions model = Sub.none

main = program
  {
    init = init,
    view = view,
    subscriptions = subscriptions,
    update = update
  }
