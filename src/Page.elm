module Page exposing (Model, Msg(..), init, update, view)

import Html exposing (..)
import Http
import Json.Decode exposing (..)
import Markdown
import Task

import Material


type alias Model =
  {
    mdl : Material.Model,
    path : String,
    query : String,
    root : String,
    title : String,
    contentType : String,
    contentFile : String,
    content : String
  }


type Msg =
  Mdl Material.Msg |
  PageInfoFetchSucceed String |
  PageInfoFetchFail Http.Error |
  PageContentFetchSucceed String |
  PageContentFetchFail Http.Error


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
      content = ""
    },
    Task.perform
      PageInfoFetchFail
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
  PageContentFetchSucceed pageContent ->
    (
      { model | content = pageContent },
      Cmd.none
    )
  PageContentFetchFail error ->
    (
      { model | content = "Page content do'nt loaded." },
      Cmd.none
    )
  _ ->
    (
      model, Cmd.none
    )


view : Model -> Html Msg
view model =
  article []
    [
      Markdown.toHtml [] model.content,
      hr [] [],
      div [] [ text model.contentType ],
      div [] [ text model.contentFile ]
    ]
