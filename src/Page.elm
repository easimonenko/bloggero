module Page exposing (Model, Msg(..), init, update, view)

import Html exposing (..)
import Http
import Json.Decode exposing (..)
import Markdown
import Task


type alias Model =
  {
    path : String,
    query : String,
    title : String,
    content : String
  }


type Msg =
  PageFetchSucceed String |
  PageFetchFail Http.Error


init : String -> String -> String -> (Model, Cmd Msg)
init path query root =
  (
    {
      path = path,
      query = query,
      title = "",
      content = ""
    },
    Task.perform PageFetchFail PageFetchSucceed (Http.getString <| root ++ path ++ "/index.json")
  )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  PageFetchSucceed pageInfo ->
    let
      pageTitle = case decodeString ( "title" := string ) pageInfo of
        Ok str -> str
        Err _ -> ""
    in
      (
        { model | title = pageTitle },
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
      h1 [] [ text model.title ],
      Markdown.toHtml [] "<i>Content</i>"
    ]
