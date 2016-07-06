module Page exposing (Model, Msg(..), init, update, view)

import Html exposing (..)
import Http
import Json.Decode exposing (..)
import Json.Encode
import Markdown
import Task
import VirtualDom

import Material
import Material.Button as Button
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Options as Options


type alias Model =
  {
    mdl : Material.Model,
    path : String,
    query : String,
    root : String,
    title : String,
    contentType : String,
    contentFile : String,
    content : VirtualDom.Node Msg
  }


type Msg =
  Mdl Material.Msg |
  PageInfoFetchSucceed String |
  PageInfoFetchFail { path : String, query : String } Http.Error |
  PageContentFetchSucceed String |
  PageContentFetchFail Http.Error |
  ButtonPageInfoRefresh { path : String, query : String }


init : String -> String -> String -> (Model, Cmd Msg)
init path query root =
  (
    {
      mdl = Material.model,
      path = path,
      query = query,
      root = root,
      title = "",
      contentType = "",
      contentFile = "",
      content = text ""
    },
    Task.perform
      (PageInfoFetchFail { path = path, query = query })
      PageInfoFetchSucceed
      (Http.getString <| root ++ path ++ "/index.json")
  )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  Mdl mdlMsg ->
    Material.update Mdl mdlMsg model
  PageInfoFetchSucceed pageInfo ->
    let
      pageTitle =
        case decodeString ( "title" := string ) pageInfo of
          Ok str -> str
          Err _ -> ""
      (contentType, contentFile) =
        case decodeString ( "content" := ( object2 (,) ( "type" := string ) ( "file" := string ) ) ) pageInfo of
          Ok res -> res
          Err _ -> ("markdown", "index.markdown")
    in
      (
        { model | title = pageTitle, contentType = contentType, contentFile = contentFile },
        Task.perform
          PageContentFetchFail
          PageContentFetchSucceed
          (Http.getString <| model.root ++ model.path ++ "/" ++ contentFile)
      )
  PageInfoFetchFail pageUrl Http.NetworkError ->
    (
      {
        model |
          title = "Network Error",
          content =
            Options.div []
              [
                h1 [] [ text "Network Error" ],
                Options.div [Elevation.e2, Color.background (Color.color Color.Yellow Color.S50)]
                  [
                    p [] [ text "Network error: try refreshing the page later." ],
                    p []
                    [
                      Button.render Mdl [0] model.mdl
                        [
                          Button.raised,
                          Button.colored,
                          Button.ripple,
                          Button.onClick (ButtonPageInfoRefresh pageUrl)
                        ]
                        [
                          text "Refresh"
                        ]
                    ]
                  ]
              ]
      },
      Cmd.none
    )
  PageInfoFetchFail pageUrl Http.Timeout ->
    (
      {
        model |
          title = "Http Timeout",
          content =
            Options.div []
              [
                h1 [] [ text "Http Timeout" ],
                Options.div [Elevation.e2, Color.background (Color.color Color.Yellow Color.S50)]
                  [
                    p [] [ text "Http timeout: try refreshing the page later." ],
                    p []
                    [
                      Button.render Mdl [0] model.mdl
                        [
                          Button.raised,
                          Button.colored,
                          Button.ripple,
                          Button.onClick (ButtonPageInfoRefresh pageUrl)
                        ]
                        [
                          text "Refresh"
                        ]
                    ]
                  ]
              ]
      },
      Cmd.none
    )
  PageContentFetchSucceed pageContent ->
    (
      { model | content = article [] [ Markdown.toHtml [] pageContent ] },
      Cmd.none
    )
  PageContentFetchFail error ->
    (
      { model | content = div [] [ text "The content of the page was not loaded." ] },
      Cmd.none
    )
  ButtonPageInfoRefresh pageUrl ->
    (
      model,
      Task.perform
        (PageInfoFetchFail pageUrl)
        PageInfoFetchSucceed
        (Http.getString <| model.root ++ pageUrl.path ++ "/index.json")
    )
  _ ->
    (
      model, Cmd.none
    )


view : Model -> Html Msg
view model = model.content
